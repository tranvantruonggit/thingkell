#include "Rts.h"
#include <stdint.h>
extern uint32_t weird_adder( uint32_t a, uint32_t b);

/* thk for thingkell */
extern uint32_t thk_xor32( uint32_t a, uint32_t b);
extern uint16_t thk_xor16( uint32_t a, uint32_t b);
extern uint8_t thk_xor8( uint32_t a, uint32_t b);

/* And operation */
extern uint8_t thk_and8( uint32_t a, uint32_t b);
extern uint16_t thk_and16( uint32_t a, uint32_t b);
extern uint32_t thk_and32( uint32_t a, uint32_t b);

extern uint32_t thk_or32( uint32_t a, uint32_t b);

/* Shift */
extern uint8_t thk_shl8( uint32_t a, uint8_t b);
extern uint16_t thk_shl16( uint32_t a, uint8_t b);
extern uint32_t thk_shl32( uint32_t a, uint8_t b);
extern uint32_t thk_shl( uint32_t a, uint8_t b);

extern uint32_t thk_shr( uint32_t a, uint8_t b);

/* Get nth bit */
extern uint32_t thk_tstb( uint32_t val, uint8_t pos);

