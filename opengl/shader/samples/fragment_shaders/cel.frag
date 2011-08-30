varying vec3 normal, light_dir;

/*!
 * \brief   Cel Shading
 *
 * Using light direction with dot product on normal of fragment, we
 * getting the intensity of light on the fragment (between [0,1]).
 * Using this intensity with threshold function to getting color.
 */

float threshold(float intensity)
{
  if(intensity > 0.5) {
    return 1.0;
  } else if(intensity > 0.05) {
    return 0.5;
  } else {
    return 0.15;
  }
}

vec4 cel_shading(vec4 color)
{
  float intensity, factor;

  intensity = dot(light_dir, normalize(normal));
  factor = threshold(intensity);

  return color * vec4(factor, factor, factor, 1.);
}

void main(void)
{
  vec4 color;

  color = vec4(.5, .0, .5, 1.);

  color = cel_shading(color);

  gl_FragColor = color;
}

