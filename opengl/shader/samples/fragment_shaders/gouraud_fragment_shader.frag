varying vec3 normal, light_dir;

/*!
 * \brief   Gouraud Shading
 *
 * Using light direction with dot product on normal of fragment, we
 * getting the intensity of light on the fragment (between [0;1]).
 * Using this intensity for getting right color.
 */
vec4 gouraud_shading(vec4 color)
{
  float intensity;

  intensity = dot(light_dir, normalize(normal));

  return color * vec4(intensity, intensity, intensity, 1.);
}

void main(void)
{
  vec4 color;

  color = vec4(.5, .0, .5, 1.);

  color = gouraud_shading(color);

  gl_FragColor = color;
}

