from machine import Pin, Timer
led = Pin(25, Pin.OUT)
led.value(1)
# timer = Timer()

# def blink(timer):
#    led.toggle()
    
# timer.init(freq=2.5, mode=Timer.PERIODIC, callback=blink)    