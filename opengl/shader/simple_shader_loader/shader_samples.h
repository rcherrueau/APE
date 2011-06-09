/*!
 * \file    shader_samples.h
 * \brief   Functions to call some shaders on one render.
 *
 *
 * \author  Ronan-Alexandre Cherrueau ronancherrueau{at}gmail{dot}com
 * \date    last modified 09/06/2011
 * \date    first release 06/06/2011
 */

#ifndef __SHADER_SAMPLES_H__
#define __SHADER_SAMPLES_H__

#include "shader_compiling.h"

//! \brief  Load trivial shader sample.
void trivial_shader(void);

//! \brief Load shader, using Gouraud for render.
void gouraud_shader(void);

//! \brief Load shader, using cel-shading for render.
void cel_shader(void);

/*!
 * \brief  Load uniform shader sample.
 *
 * Sample show how to pass uniform variable. Uniform variable
 * are just readable variable for the shader but pass from program.
 */
void uniform_shader(void);

//! \var Resources of shader.
shader_resources sc;

#endif

