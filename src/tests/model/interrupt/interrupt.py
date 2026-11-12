import signal
import time
import gamspy as gp

print("starting", flush=True)
# Flag to indicate if signal was received
interrupted = False

def handle_sigint(signum, frame):
    global interrupted
    print("interrupt received", flush=True)
    interrupted = True

# Register the SIGINT handler
signal.signal(signal.SIGINT, handle_sigint)

# Wait up to 20 seconds
start_time = time.time()
while time.time() - start_time < 10:
    if interrupted:
        start_time = time.time() - 6
        interrupted = False
    time.sleep(0.1)  # Sleep briefly to reduce CPU usage

c = gp.Container()
i = gp.Set(c, "i", records=[f"i{x+1}" for x in range(10)])
test = gp.Parameter(c, "test", domain=i, records=[("i2", 10), ("i5", 50)], is_miro_output=True)
print("exiting", flush=True)
