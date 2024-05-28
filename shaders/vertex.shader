#version 460 core

layout(location = 0) in vec2 vPosition;
layout(location = 1) in vec4 vcolor;
layout(location = 2) in mat4 transform;

uniform mat4 projection;

out vec4 c;

void main()
{
  gl_Position = projection * (vec4(vPosition.xy, 0.0, 1.0) * transform);
  c = vcolor;
}