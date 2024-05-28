#version 460 core

// Ouput data
out vec4 fColor;
  
in vec4 c;


void main()
{
  // vec4 coordpos =  gl_FragCoord;
  fColor = c;
}