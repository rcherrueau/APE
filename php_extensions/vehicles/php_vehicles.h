/*!
 * \brief   Php Extension Writing with Zend to wrapp c++ classes.
 *
 * This file is the implementation of Wrapping C++ Classes in a PHP Extension by
 * Paul Osman.
 *
 * \see     http://devzone.zend.com/article/4486
 */
#ifndef PHP_VEHICLES_H
#define PHP_VEHICLES_H

#define PHP_VEHICLES_VERSION "0.1"
#define PHP_VEHICLES_EXTNAME "vehicles"

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "php.h"

extern zend_module_entry vehicles_module_entry;
#define phpext_vehicles_ptr &vehicles_module_entry;

#endif

