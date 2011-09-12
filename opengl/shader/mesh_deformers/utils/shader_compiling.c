/*!
 * \file    shader_compiling.c
 * \brief   Read shader_compiling.h for more information.
 *
 * \author  Ronan-Alexandre Cherrueau ronancherrueau{at}gmail{dot}com
 * \date    last modified 20/05/2011
 * \date    first release 18/05/2011
 */

#include "shader_compiling.h"

char *load_source(const char *filename)
{
  char *src = NULL;
  FILE  *fp = NULL;
  long int size;
  long int i;

  fp = fopen(filename, "r");
  if(fp == NULL) {
    fprintf(stderr, "Impossible d'ouvrir le fichier '%s'.\n", filename);
    return NULL;
  }

  fseek(fp, 0, SEEK_END);
  size = ftell(fp);

  rewind(fp);

  src = malloc(size + 1);
  if(src == NULL) {
    fprintf(stderr, "Erreur d'allocation mémoire.\n");
    fclose(fp);
    return NULL;
  }

  for(i = 0; i < size; ++i) {
    src[i] = fgetc(fp);
  }
  src[size] = '\0';

  fclose(fp);

  return src;
}

void show_info_log(GLuint object, PFNGLGETSHADERIVPROC glGet__iv,
    PFNGLGETSHADERINFOLOGPROC glGet__InfoLog)
{
  GLsizei log_length = 0;
  char *log = NULL;

  glGet__iv(object, GL_INFO_LOG_LENGTH, &log_length);
  log = malloc(log_length + 1);
  if(log == NULL) {
    fprintf(stderr, "Erreur d'allocation mémoire.\n");
    exit(0);
  }

  memset(log, '\0', log_length + 1);
  glGet__InfoLog(object, log_length, &log_length, log);
  fprintf(stderr, "%s\n", log);
  if(log){ free(log); log = NULL; }
}

GLuint load_shader(GLenum type, const char *filename)
{
  char *src = NULL;
  GLuint shader = 0;
  GLint compile_status = GL_TRUE;

  // Create shader.
  shader = glCreateShader(type);
  if(shader == 0) {
    fprintf(stderr, "Impossible de creer le shader.\n");
    return 0;
  }

  // Load source code.
  src = load_source(filename);
  if(src == NULL) {
    glDeleteShader(shader);
    return 0;
  }

  glShaderSource(shader, 1, (const GLchar **) &src, NULL);
  if(src){ free(src); src = NULL; }

  glCompileShader(shader);

  // Test compiling success.
  glGetShaderiv(shader, GL_COMPILE_STATUS, &compile_status);
  if(compile_status != GL_TRUE) {
    // On Error, getting log.
    fprintf(stderr, "Impossible de compiler le shader '%s':\n", filename);
    show_info_log(shader, glGetShaderiv, glGetShaderInfoLog);
    glDeleteShader(shader);
    return 0;
  }

  return shader;
}

GLint make_program_from_one(GLuint shader)
{
  GLuint program = 0;
  GLint linking_status = GL_TRUE;

  // Creation d'un program.
  program = glCreateProgram();
  if(program == 0) {
    fprintf(stderr, "Impossible de créer le program.\n");
    return 0;
  }

  // Attache des shaders et liage du program.
  glAttachShader(program, shader);
  glLinkProgram(program);

  // Vérification du succes du liage.
  glGetProgramiv(program, GL_LINK_STATUS, &linking_status);
  if(linking_status != GL_TRUE) {
    fprintf(stderr, "Impossible de lier les shaders au program :\n");
    show_info_log(program, glGetProgramiv, glGetProgramInfoLog);
    glDeleteProgram(program);
    return 0;
  }

  return program;
}

GLint make_program_from_two(GLuint vertex_shader, GLuint fragment_shader)
{
  GLuint program = 0;
  GLint linking_status = GL_TRUE;

  // Create program.
  program = glCreateProgram();
  if(program == 0) {
    fprintf(stderr, "Impossible de créer le program.\n");
    return 0;
  }

  // Attaching shaders and linking program.
  glAttachShader(program, vertex_shader);
  glAttachShader(program, fragment_shader);
  glLinkProgram(program);

  // Test linking success.
  glGetProgramiv(program, GL_LINK_STATUS, &linking_status);
  if(linking_status != GL_TRUE) {
    fprintf(stderr, "Impossible de lier les shaders au program :\n");
    show_info_log(program, glGetProgramiv, glGetProgramInfoLog);
    glDeleteProgram(program);
    return 0;
  }

  return program;
}

