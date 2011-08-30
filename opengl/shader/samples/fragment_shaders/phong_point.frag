/*!
 * \brief   Phong Ligthning.
 * 
 * In Phong Lightning, the final color of one pixel is compute as :
 * <tt>final_color = abient_term + diffuse_term + specular_term</tt>
 * with:
 *  \li \c <tt>ambient_term = ambient_ligh * ambient_material</tt> is a constant
 *  color added to all points which is a crude global approximation to indirect
 *  lighting ;
 *  \li <tt>diffuse_term = diffuse_light * diffuse_material * lambert_term</tt>
 *  with \c lambert_term is intensity of might on materiel. This term realy
 *  emboss the object.
 *  \li <tt>specular_term = specular_light * specular_material *
    pow( max( dot (reflect(-light, normal), eye), 0.0), shininess)</tt>
 *
 * In this fragment shader we use gl_FrontLightProduct[] which is derived state
 * from products of ligth and material (gl_LightSource[] * gl_FrontMaterial);
 */
varying vec3 normal, vertex_vector;

void main(void)
{
  float specular;
  vec3 light, eye, light_normal_reflect;
  vec4 ambient_term, diffuse_term, specular_term;

  light = normalize(vec3(gl_LightSource[0].position) - vertex_vector);


  // Compute Ambient Term:
  //  * ambient_light = gl_LightSource[0].ambient
  //  * ambient_material = gl_FrontMaterial.ambient;
  ambient_term = gl_FrontLightProduct[0].ambient;

  // Compute Diffuse Term:
  //  * diffuse_light = gl_LightSource[0].diffuse;
  //  * diffuse_material = gl_FrontMaterial.diffuse;
  //  * lambert_term = max(dot(normal, light), 0.);
  diffuse_term = gl_FrontLightProduct[0].diffuse * max(dot(normal, light), 0.);
  diffuse_term = clamp(diffuse_term, 0., 1.);

  // Compute Specular Term:
  //   * we are in Eye Coordinates, so EyePos is (0,0,0)
  eye = normalize(-vertex_vector);
  light_normal_reflect = normalize(reflect(-light, normal));
  specular = pow(max(dot(light_normal_reflect, eye), 0.),
    gl_FrontMaterial.shininess);
  specular_term = gl_FrontLightProduct[0].specular * specular;
  specular_term = clamp(specular_term, 0., 1.);

  gl_FragColor = ambient_term + diffuse_term + specular_term;
}

