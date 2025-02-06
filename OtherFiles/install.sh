#!/bin/bash

# Ensure the script is run with root privileges
if [[ $EUID -ne 0 ]]; then
    echo "This script must be run as root. Try using sudo."
    exit 1
fi

# Define source and target locations
SOURCE_DIR="$(dirname "$(realpath "$0")")"
SERVICE_FILE="$SOURCE_DIR/kanata.service"
TARGET_1="/etc/systemd/kanata.service"
TARGET_2="/etc/systemd/multi-user.target.wants/kanata.service"

# Check if the service file exists
if [[ ! -f "$SERVICE_FILE" ]]; then
    echo "Error: kanata.service file not found in $SOURCE_DIR"
    exit 1
fi

# Copy the service file to both locations
cp "$SERVICE_FILE" "$TARGET_1"
cp "$SERVICE_FILE" "$TARGET_2"

# Set proper permissions
chmod 644 "$TARGET_1" "$TARGET_2"

# Reload systemd daemon to recognize the new service
systemctl daemon-reload

# Enable the service so it starts on boot
systemctl enable kanata.service

# Print success message
echo "kanata.service successfully copied and enabled."
