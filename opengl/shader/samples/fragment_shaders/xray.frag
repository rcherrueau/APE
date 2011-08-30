/*!
 * \file    xray_fragment_shader.frag
 * \brief   Fragment shader for XRay Shading.
 *
 * \author  Ronan-Alexandre Cherrueau ronancherrueau{at}gmail{dot}com
 * \date    last modified 24/08/2011
 * \date    first release 24/08/2011
 */

varying vec3 normal, light_dir;

//! \struct    xray shading values
struct xray_t
{
  vec4 color;     //!< New pixel color after xray shading;
  float alpha;    //!< New pixel alpha transparency;
};

xray_t xray_shading(vec4 color)
{
  float intensity, opacity;
  xray_t xray;

  intensity = dot(normalize(light_dir), normalize(normal));
  opacity = 1. - intensity;

  xray.color = color * vec4(opacity, opacity, opacity, 1.);
  xray.alpha = opacity;

  return xray;
}

void main(void)
{
  vec4 color;
  xray_t xray;

  color = vec4(0., .5, .5, 1.);
  xray = xray_shading(color);

  gl_FragColor = xray.color;
  gl_FragColor.a = xray.alpha;
}

