#version 460 core

// Ouput data
out vec4 fColor;
  
uniform vec4 vcolor;

void main()
{
  // vec4 coordpos =  gl_FragCoord;
  // vec4 color2 = vec4(0.7, 0.7, 0.7, 1.0);

  // vec2 st = gl_PointCoord;

  // float mixValue = distance(st, vec2(0, 1));
  // vec4 color3 = mix(vcolor, color2, mixValue);
    
  gl_FragColor = vcolor;
}