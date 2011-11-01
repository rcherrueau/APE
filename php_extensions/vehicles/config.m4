PHP_ARG_ENABLE(vehicles,
  [ --enable-vehicles   Enable Vehicles support])

if test "$PHP_VEHICLES" = "yes"; then
  PHP_REQUIRE_CXX()
  PHP_SUBST(VEHICLES_SHARED_LIBADD)
  PHP_ADD_LIBRARY(stdc++, 1, VEHICLES_SHARED_LIBADD)
  PHP_NEW_EXTENSION(vehicles, vehicles.cpp car.cpp, $ext_shared)
fi

