/*!
 * \brief   Php Extension Writing with Zend to wrapp c++ classes.
 *
 * This file is the implementation of Wrapping C++ Classes in a PHP Extension by
 * Paul Osman.
 *
 * \see     http://devzone.zend.com/article/4486
 */
#include "php_vehicles.h"

zend_module_entry vehicles_module_entry = {
#if ZEND_MODULE_API_NO >= 20010901
  STANDARD_MODULE_HEADER,
#endif
  PHP_VEHICLES_EXTNAME,
  NULL,                   //!< Functions
  PHP_MINIT(vehicles),
  NULL,                   //!< MSHUTDOWN
  NULL,                   //!< RINIT
  NULL,                   //!< RSHUTDOWN
  NULL,                   //!< MINFO
#if ZEND_MODULE_API_NO >= 20010901
  PHP_VEHICLES_EXTNAME,
#endif
  STANDARD_MODULE_PROPERTIES
};

#ifdef COMPILE_DL_VEHICLES
ZEND_GET_MODULE(vehicles)
#endif

PHP_MINIT_FUNCTION(vehicles)
{
  return SUCCESS;
}

