/*!
 * \brief   Function for render scene on mesh deformers
 *
 *
 * \author  Ronan-Alexandre Cherrueau ronancherrueau{at}gmail{dot}com
 * \date    first release 23/09/2011
 */

#ifndef __MESH_DEFORMERS_H__
#define __MESH_DEFORMERS_H__

#include "utils/shader_compiling.h"

/*!
 * \brief   Initialize light environement and material refllexion
 *
 * \param   x   The x position for light.
 * \param   y   The y position for light.
 * \param   z   The z position for light.
 */
void init_light(float x, float y, float z);

/*!
 * \brief   Initialize the phong point shader.
 *
 * The function is required before call \c use_phong_shader.
 */
void init_phong_point(void);

/*!
 * \brief   Initialize the mesh deformers shader.
 *
 * Mesh deformers shader use the sinus function to move vertex and
 * phong point to render light.
 *
 * The function is required before call \c use_mesh_deformers_shader.
 */
void init_mesh_deformers(void);

//! \brief  Free all alocated resources for shaders.
void exit_program(void);


//! \brief  Draw cartesian coordinates on origine.
void draw_cartesian_coordinates(void);

/*!
 * \brief   Draw a square.
 *
 * Draw a square of width \c width and height \c height and center it on
 * origine. Square have <tt>slice * slices * 2 vertex</tt>.
 *
 * \param   width   The width of square.
 * \param   height  The height of square.
 * \param   slices  Specifies number of subdivision along x and z axis.
 */
void draw_square(float width, float height, int slices);

//! \brief  Use the phong point shader.
void use_phong_shader(void);

/*!
 * \brief   Use the mesh deformers.
 *
 * \param   amplitude   Amplitude of sinus function (if 1 amplitude is [-1;1]).
 * \param   frequency   Frequancy of sinus function (if 1 frequency is 2*PI).
 */
void use_mesh_deformers_shader(float amplitude, float frequency);

//! \var shader_phong Record for phong point shader.
shader_resources shader_phong;

//! \var shader_mdeformers Record for mesh deformers.
shader_resources shader_mdeformers;

#endif

