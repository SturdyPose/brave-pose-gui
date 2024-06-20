module Graphics.ShaderLoader (
   ShaderSource(..), ShaderInfo(..), loadShaders, compileShaders
) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Graphics.Rendering.OpenGL
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Map.Strict as M
import qualified System.Directory as SysDir
import Data.Char (toLower)
import Data.List (sort, sortBy)

--------------------------------------------------------------------------------

-- | The source of the shader source code.

data ShaderSource =
     ByteStringSource B.ByteString
     -- ^ The shader source code is directly given as a 'B.ByteString'.
   | StringSource String
     -- ^ The shader source code is directly given as a 'String'.
   | FileSource FilePath
     -- ^ The shader source code is located in the file at the given 'FilePath'.
   deriving ( Eq, Ord, Show )

getSource :: ShaderSource -> IO B.ByteString
getSource (ByteStringSource bs) = return bs
getSource (StringSource str) = return $ packUtf8 str
getSource (FileSource path) = B.readFile path

--------------------------------------------------------------------------------

-- | A description of a shader: The type of the shader plus its source code.

data ShaderInfo = ShaderInfo ShaderType ShaderSource
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

-- | Create a new program object from the given shaders, throwing an
-- 'IOException' if something goes wrong.

loadShaders :: [ShaderInfo] -> IO Program
loadShaders infos =
   createProgram `bracketOnError` deleteObjectName $ \program -> do
      loadCompileAttach program infos
      linkAndCheck program
      return program

linkAndCheck :: Program -> IO ()
linkAndCheck = checked linkProgram linkStatus programInfoLog "link"

loadCompileAttach :: Program -> [ShaderInfo] -> IO ()
loadCompileAttach _ [] = return ()
loadCompileAttach program (ShaderInfo shType source : infos) =
   createShader shType `bracketOnError` deleteObjectName $ \shader -> do
      src <- getSource source
      shaderSourceBS shader $= src
      compileAndCheck shader
      attachShader program shader
      loadCompileAttach program infos

compileAndCheck :: Shader -> IO ()
compileAndCheck = checked compileShader compileStatus shaderInfoLog "compile"

checked :: (t -> IO ())
        -> (t -> GettableStateVar Bool)
        -> (t -> GettableStateVar String)
        -> String
        -> t
        -> IO ()
checked action getStatus getInfoLog message object = do
   action object
   ok <- get (getStatus object)
   unless ok $ do
      infoLog <- get (getInfoLog object)
      fail (message ++ " log: " ++ infoLog)


shaderNameToShaderType:: String -> GL.ShaderType 
shaderNameToShaderType x = case map toLower x of 
   "vertex.shader" -> GL.VertexShader
   "tessellationcontrol.shader" -> GL.TessControlShader
   "tessellationeval.shader" -> GL.TessEvaluationShader
   "geometry.shader" -> GL.GeometryShader
   "fragment.shader" -> GL.FragmentShader
   "computeshader.shader" -> GL.ComputeShader
   _ -> error $ "Shader type is unknown: (" ++ x ++ "). Only known types are: Vertex | Tessellation | Geometry | Fragment"

-- TODO: Make this compile time and output program into a binary
-- This compiles shaders from "shaders" directory and compiles a program based on the subdirectory name.
-- Result is a Map. So if you want to grab a specific shader, call it by subdirectory name.
-- Also the name of shaders itself should be based on the pattern match inside shaderNameToShaderType
compileShaders:: IO (M.Map B.ByteString GL.Program)
compileShaders = do
   -- let instancedShaders = [
   --             ShaderInfo GL.VertexShader   (FileSource "shaders/InstancedShaders/vertex.shader"),
   --             ShaderInfo GL.FragmentShader (FileSource "shaders/InstancedShaders/fragment.shader")
   --       ]

   -- instancedProgram <- loadShaders instancedShaders

   shaderDirectories <- SysDir.listDirectory "shaders" 

   let shaderSort (ShaderInfo t1 _) (ShaderInfo t2 _) = compare t1 t2 

   pairedShaders <- mapM (\dirName -> do 
      pairedShader <- (\shaderNames -> sortBy shaderSort $ map (\shaderName -> ShaderInfo (shaderNameToShaderType shaderName) (FileSource ("shaders/"++dirName++"/"++shaderName))) shaderNames) <$> SysDir.listDirectory ("shaders/"++dirName)
      program <- loadShaders pairedShader 
      return (B8.pack dirName, program)
      ) shaderDirectories

   return $ M.fromList pairedShaders 
