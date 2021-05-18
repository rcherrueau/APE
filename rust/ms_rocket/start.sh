#!/usr/bin/env sh
set -x

BIN_DIR=./target/x86_64-unknown-linux-musl/release

# Compile all services into statically linked bins with musl
nix-shell --command 'cargo build --release $MUSL_TARGET'

# Kill all running services
if ! fgrep -q "kill-session" <<< $(tmux list-sessions)
then  
  tmux kill-session -t ms_rocket 
fi  

# Create a tmux session to run services
tmux new-session -d -s ms_rocket

# Run services in the tmux session
tmux new-window -d -t ms_rocket -n identity $BIN_DIR/identity
tmux new-window -d -t ms_rocket -n images $BIN_DIR/images
tmux new-window -d -t ms_rocket -n compute $BIN_DIR/compute

# Attach to the tmux
tmux attach-session -t ms_rocket

