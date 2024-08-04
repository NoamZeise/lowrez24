#version 330

in vec3 position;
in vec3 normal;

out vec4 colour;

uniform vec3 cam_pos;

void main() {
  vec3 p = (position + vec3(3)) * 0.2;
  colour = vec4(p.x, 1, 1, 1);
  // shading constants
  vec3 object_colour = vec3(1);
  vec3 cool = vec3(0.2, 0, 0.55) + 0.25*object_colour;
  vec3 warm = vec3(0.5, 0.3, 0) + 0.5*object_colour;
  vec3 highlight = vec3(1, 0.8, 0.2);
  float specular = 30.0f;
  
  vec3 n = normalize(normal);
  vec3 l = -vec3(0.5, 0.2, -0.7); // light direction
  vec3 v = normalize(cam_pos - position);

  // gooch shading
  float t = (dot(n,l) + 1)/2.0;
  vec3 r = -reflect(l,n);
  float s = clamp(pow(dot(r,v), specular), 0, 1);
  vec3 base = mix(cool, warm,  t);
  vec3 shaded = mix(base, highlight, s);
  
  // edge outline
  float edge_amount = dot(n, v);
  float thickness = 0.6;
  edge_amount = clamp(edge_amount, 0, thickness)*(1/thickness);
  edge_amount *= edge_amount * edge_amount;
  vec3 edge = vec3(edge_amount);

  colour = vec4(shaded*edge, 1);
}
