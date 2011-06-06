uniform float spec_intensity;
uniform vec4  spec_color;
uniform float t[2];
uniform vec4  colors[3];

varying vec3 normal, light_dir;

void main(void)
{
  float intensity;
  vec3  n;
  vec4  color;

  n = normalize(normal);
  intensity = max(dot(light_dir, n), 0.);

  if(intensity > spec_intensity) {
    color = spec_color;
  } else if(intensity > t[0]) {
    color = colors[0];
  } else if(intensity > t[1]) {
    color = colors[1];
  } else {
    color = colors[2];
  }

  gl_FragColor = color;
}

