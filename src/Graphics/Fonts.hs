{-# LANGUAGE OverloadedStrings #-}

module Graphics.Fonts where 
import Data.Foldable (foldrM)
import Data.ByteString (ByteString, split, splitWith)
import qualified Data.Vector as V
import Foreign (alloca, Storable (..))
import FreeType (FT_ULong, ft_Done_Face)
import qualified Linear.V2 as Linear
import Data.Word (Word32)
import Data.Int (Int32, Int64)
import Graphics.GL (glGenTextures, glBindTexture)
import qualified Graphics.Rendering.OpenGL as GL
import FreeType 
import qualified Data.Map.Strict as M
import System.Directory (listDirectory)
import System.FilePath (takeFileName)
import qualified Graphics.GL as GL


type FontPath = String 

data FreeTypeCharacter = FreeTypeCharacter {
    -- https://learnopengl.com/In-Practice/Text-Rendering
      _freeTypeCharacterTextureID :: !GL.TextureObject
    , _freeTypeCharacterSize      :: !(Linear.V2 Int)
    , _freeTypeCharacterBearing   :: !(Linear.V2 Int)
    , _freeTypeCharacterAdvance   :: !(Linear.V2 Int)
} deriving Show

initLoadFontFileGL:: FontPath -> FT_Library -> IO (String -> [FreeTypeCharacter])
initLoadFontFileGL path lib = do
    face <- ft_New_Face lib path 0

    ft_Set_Pixel_Sizes face 0 48

    ft_Select_Charmap face FT_ENCODING_UNICODE
    (charCode, glyphIndex) <- ft_Get_Next_Char face Nothing

    ft_Load_Char face charCode FT_LOAD_RENDER

    faceVal <- peek face -- TODO: check for nullptr
    let pglyph = frGlyph faceVal

    glyphVal <- peek pglyph -- TODO: check for nullptr
    let bitmapVal = gsrBitmap glyphVal

    texture <- GL.genObjectName :: IO GL.TextureObject
    GL.glPixelStorei GL.GL_UNPACK_ALIGNMENT 1
    GL.textureBinding GL.Texture2D GL.$= Just texture

    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.R8 (GL.TextureSize2D (toEnum . fromEnum $ bWidth bitmapVal) (toEnum . fromEnum $ bRows bitmapVal)) 0 (GL.PixelData GL.Red GL.UnsignedByte (bBuffer bitmapVal))
    GL.textureFilter   GL.Texture2D      GL.$= ((GL.Linear', Nothing), GL.Linear')
    GL.textureWrapMode GL.Texture2D GL.S GL.$= (GL.Repeated, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T GL.$= (GL.Repeated, GL.ClampToEdge)

    let freeTypeChar = FreeTypeCharacter {
          _freeTypeCharacterTextureID = texture
        , _freeTypeCharacterSize      = Linear.V2 ((toEnum . fromEnum) (bWidth bitmapVal)) ((toEnum . fromEnum) (bRows bitmapVal))
        , _freeTypeCharacterBearing   = Linear.V2 ((toEnum . fromEnum) (gsrBitmap_left glyphVal)) ((toEnum . fromEnum) (gsrBitmap_top glyphVal))
        , _freeTypeCharacterAdvance   = Linear.V2 ((toEnum . fromEnum) ((vX . gsrAdvance) glyphVal)) ((toEnum . fromEnum) ((vY . gsrAdvance) glyphVal))
    }

    charMap <- M.fromList <$> glyphsAndFaces face charCode [(charCode, freeTypeChar)]

    GL.textureBinding GL.Texture2D GL.$= Nothing
    ft_Done_Face face

    return $ \inputText -> foldr (\x acc -> case M.lookup ((toEnum . fromEnum) x) charMap of
                                                Just found -> found:acc 
                                                Nothing -> acc) [] inputText
         

    where 
        glyphsAndFaces:: FT_Face -> FT_ULong -> [(FT_ULong, FreeTypeCharacter)] -> IO [(FT_ULong, FreeTypeCharacter)]
        glyphsAndFaces face previousCharCode acc = do
            (charCode, glyphIndex) <- ft_Get_Next_Char face (Just previousCharCode) 
            if charCode /= 0 
                then do 
                    ft_Load_Char face charCode FT_LOAD_RENDER

                    faceVal <- peek face -- TODO: check for nullptr
                    let pglyph = frGlyph faceVal
                    glyphVal <- peek pglyph -- TODO: check for nullptr
                    let bitmapVal = gsrBitmap glyphVal
                    
                    newTex <- GL.genObjectName :: IO GL.TextureObject
                    GL.textureBinding GL.Texture2D GL.$= Just newTex 

                    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.R8 (GL.TextureSize2D (toEnum . fromEnum $ bWidth bitmapVal) (toEnum . fromEnum $ bRows bitmapVal)) 0 (GL.PixelData GL.Red GL.UnsignedByte (bBuffer bitmapVal))
                    GL.textureFilter   GL.Texture2D      GL.$= ((GL.Linear', Nothing), GL.Linear')
                    GL.textureWrapMode GL.Texture2D GL.S GL.$= (GL.Repeated, GL.ClampToEdge)
                    GL.textureWrapMode GL.Texture2D GL.T GL.$= (GL.Repeated, GL.ClampToEdge)

                    let freeTypeChar = FreeTypeCharacter {
                          _freeTypeCharacterTextureID = newTex 
                        , _freeTypeCharacterSize      = Linear.V2 ((toEnum . fromEnum) (bWidth bitmapVal)) ((toEnum . fromEnum) (bRows bitmapVal))
                        , _freeTypeCharacterBearing   = Linear.V2 ((toEnum . fromEnum) (gsrBitmap_left glyphVal)) ((toEnum . fromEnum) (gsrBitmap_top glyphVal))
                        , _freeTypeCharacterAdvance   = Linear.V2 ((toEnum . fromEnum) ((vX . gsrAdvance) glyphVal)) ((toEnum . fromEnum) ((vY . gsrAdvance) glyphVal))
                    }
                    glyphsAndFaces face charCode ((charCode, freeTypeChar):acc)
                else return acc

type FreeTypeMapping = String -> String -> [FreeTypeCharacter]

-- Should be called after initialization of opengl context
-- paths = path to fonts
initFreeType:: IO FreeTypeMapping 
initFreeType = do

    lib <- ft_Init_FreeType
    
    fontFiles <- listDirectory "fonts"

    resMap <- foldrM (\currFile accMap -> do 
        let fileName = takeFileName currFile
        mapper <- initLoadFontFileGL ("fonts/" ++ currFile) lib
        return $ M.insert fileName mapper accMap
        ) M.empty fontFiles

    return $ \inputString fontName -> case M.lookup fontName resMap of
                                        Nothing -> error "This font doesn't exist"
                                        Just mapper -> mapper inputString
