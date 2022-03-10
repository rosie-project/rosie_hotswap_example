# ROSIE hotswap example

Example of a simulated robot using ROSIE that is able to update its code while sending messages through a ROS network.

All without interruptions or reboots, thanks to Erlang and OTP.

The "Mars Environmental Dynamics Analyzer" is an experimental weather station part of the equipment of the Perseverace Rover, sent in mission on Mars. The measurements used are calibrated data from martian day 99 of when the rover landed.

Data is taken from:
<https://pds.nasa.gov/ds-view/pds/viewBundle.jsp?identifier=urn%3Anasa%3Apds%3Amars2020_meda&version=1.0>

---

## Rviz2 Setup

The simulation is visualized through rviz2 receiving data through the ROSiE node that simulates the perseverance rover.

Rviz2 and urdf file format require absolute paths so make sure to adjust them. ( this is terrible btw, any solution to this is appreciated)

If you encounter any Rviz2 bug or crash loading the models remember that it's not necessary to display the urdf models to test the example.

## Instructions

### Clone this repository and go to tag 1.0.0

    git clone https://github.com/rosie-project/rosie_hotswap_example.git
    git checkout "1.0.0"

### Build and pack release 1.0.0

    rebar3 release
    rebar3 tar

### Place the tar ball in your installation dir, unpack and launch

    mkdir install
    cp _build/default/rel/rosie_hotswap_example/rosie_hotswap_example-1.0.0.tar.gz install
    cd install
    tar xzfv rosie_hotswap_example-1.0.0.tar.gz
    ./bin/rosie_hotswap_example-1.0.0 foreground

### Launch the provided rviz2 configuration

You will notice the rover bouncing around, instable temperature and funky wind direction arrows.

All is clearly bugged and needs a fix available in release 1.0.1.

### Switch code and build the new release 1.0.1

This version of the app fixes the bug

    git checkout "1.0.1"
    rebar3 release

### Prepare the tar ball to perform the upgrade

This example uses <https://github.com/lrascao/rebar3_appup_plugin.git> to make this process more simple

    rebar3 appup generate
    rebar3 relup --relname rosie_hotswap_example --relvsn "1.0.1"
    rebar3 tar

### Copy the tar ball in the release directory

    mkdir install/releases/1.0.1
    cp _build/default/rel/rosie_hotswap_example/rosie_hotswap_example-1.0.1.tar.gz install/releases/1.0.1

### Run the release script to upgrade the remote node

    ./install/bin/rosie_hotswap_example-1.0.0 upgrade "1.0.1"

The rover should stabilize its course and display a normal behavior.
The script launched with "upgrade" should return after printing info about the performed operations.
