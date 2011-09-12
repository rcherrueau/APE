/*!
 * \brief   Shader Compiling process
 *
 * Simple functions to laod and compil sahder with log error. 
 *
 * \sa
 http://duriansoftware.com/joe/An-intro-to-modern-OpenGL.-Table-of-Contents.html
 *
 * \author  Ronan-Alexandre Cherrueau ronancherrueau{at}gmail{dot}com
 * \date    last modified 20/05/2011
 * \date    first release 18/05/2011
 */

#ifndef __SHADER_COMPILING_H__
#define __SHADER_COMPILING_H__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <GL/glew.h>

/*!
 * \struct  shader_resources
 * \brief   Shader loader resources.
 */
typedef struct {
  //! \var  Vertex shader.
  GLuint vertex_shader;

  //! \var  Fragment(/pixel) shader.
  GLuint fragment_shader;

  //! \var  Program to compile shaders.
  GLuint program;

} shader_resources;

/*!
 * \brief Loading source code of shader.
 *
 * Getting source code of shader file and put data in string.
 *
 * \param filename  The path and filename of shader source file.
 * \retrun          The data of shader source code.
 */
char *load_source(const char *filename);

/*!
 * \brief  Print le log d'erreur.
 *
 * Display log error of object on standard output.
 *
 * \param object          GL object (shader, program, ...).
 * \param glGet__iv       Function to get length of log info.
 * \param glGet__InfoLog  Function to get log information.
 */
void show_info_log(GLuint object, PFNGLGETSHADERIVPROC glGet__iv,
    PFNGLGETSHADERINFOLOGPROC glGet__InfoLog);

/*!
 * \brief   Load a shader.
 *
 * Load specific shader from specific file and compiling it
 * with error managing.
 *
 * \param   type      Type of shader.
 * \param   filename  The path and filename of shader source file.
 * \return            The lading shader.
 */
GLuint load_shader(GLenum type, const char *filename);

/*!
 * \brief  Make program with one shader.
 *
 *
 * \param   shader  The shader.
 * \return          The program.
 */
GLint make_program_from_one(GLuint shader);

/*!
 * \brief  Make program with vertex and pixel shader.
 *
 * Creating program from vertex and fragment(/pixel) shader
 * and linking shader to program with error managing.
 *
 * \param   vertex_shader   The vertex shader.
 * \param   fragment_shader The fragment(/pixel) shader.
 * \return                  The program.
 */
GLint make_program_from_two(GLuint vertex_shader, GLuint fragment_shader);

#endif

