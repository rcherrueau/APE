#include "car.h"

const int Car::COEF = 5;

Car::Car(const int max_gear): max_gear_(max_gear), gear_(1), speed_(0)
{
}

void Car::shift(const int gear)
{
  if(gear < 1 || gear > max_gear_) { return; }
  gear_ = gear;
}

void Car::accelerate()
{
  speed_ += COEF * gear_;
}

void Car::brake()
{
  speed_ -= COEF * gear_;
}

int Car::speed() const
{
  return speed_;
}

int Car::gear() const
{
  return gear_;
}

