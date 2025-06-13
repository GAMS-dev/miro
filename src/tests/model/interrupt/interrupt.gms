$onEmbeddedCode Python:
import signal
import time
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
$offEmbeddedCode

set i /i1*i10/;
$onExternalOutput
parameter test(i) /i2 10, i5 50/
$offExternalOutput

$log exiting
