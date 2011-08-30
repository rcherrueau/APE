uniform vec4 user_color;
uniform float threshold[3];

varying vec3 normal, light_dir;

vec4 gouraud_shading(vec4 color)
{
  float intensity;

  intensity = dot(light_dir, normalize(normal));

  color *= vec4(intensity, intensity, intensity, 1.);

  return color;
}

vec4 cel_shading(vec4 color)
{
  float intensity, factor;

  intensity = dot(light_dir, normalize(normal));

  if(intensity >= threshold[0]) {
    factor = 1.0;
  } else if(intensity >= threshold[1]) {
    factor = 0.5;
  } else if(intensity >= threshold[2]) {
    factor = 0.25;
  } else {
    factor = 0.1;
  }

  return color * vec4(factor, factor, factor, 1.);
}

void main(void)
{
  vec4 color;

  color = user_color;

  // color = gouraud_shading(color);
  color = cel_shading(color);

  gl_FragColor = color;
}

