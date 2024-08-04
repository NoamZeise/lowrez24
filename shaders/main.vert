#version 330
layout (location = 0) in vec3 in_position;
layout (location = 1) in vec3 in_normal;
layout (location = 2) in vec2 in_uv;

uniform mat4 model;
uniform mat4 normal_mat;
uniform mat4 view;
uniform mat4 projection;

out vec3 position;
out vec3 normal;
//out vec2 uv;

void main() {
  vec4 world_position = model * vec4(in_position, 1);
  position = vec3(world_position);
  normal = vec3(normal_mat * vec4(in_normal, 1));
 // uv = in_uv;
  gl_Position = projection * view * world_position;
}
