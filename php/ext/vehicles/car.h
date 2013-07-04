#ifndef __VEHICLES_CAR_H__
#define __VEHICLES_CAR_H__

/*!
 * \class   Car
 * \brief   Meta-Data of car.
 *
 * A car is an object with gears and speed. More the gear have an high level
 * and more the speed will increase.
 */
class Car
{
  public:
    //! \var COEF speed ration multiplier.
    static const int COEF;

    /*!
     * \brief   Constructor of Car.
     *
     * \param   max_gear  Maximum gears number possibilities.
     */
    Car(const int max_gear);

    /*!
     * \brief   Shift the current gear.
     * 
     * \invariant gear >= 1 && gear < Car::max_gear_
     * \param   gear  The new gear to use.
     */
    void shift(const int gear);

    /*!
     * \brief   Accelerate speed of Car.
     *
     * Speed acceleration is calculated as follow, Car::COEF * Car::gear_.
     */
    void accelerate();

    /*!
     * \brief   Break speed of Car.
     *
     * Speed breaking is calculated as follow, Car::COEF * Car::gear_;
     */
    void brake();

    /*!
     * \brief   Returns the current speed of Car.
     *
     * \return  The speed.
     */
    int speed() const;

    /*!
     * \brief   Returns the current gears of Car.
     *
     * \return  The gear.
     */
    int gear() const;

  private:
    int max_gear_;    //!< Gears range.
    int gear_;        //!< The current gear;
    int speed_;       //!< The current speed;
};

#endif

