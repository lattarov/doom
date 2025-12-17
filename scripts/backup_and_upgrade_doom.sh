#!/usr/bin/env sh

# Introduction
## Script to backup the emacs directory, where doom Emacs
## binaries are installed and where all caches are stored.

# Constants
## Archive
archive_prefix="emacs"
archive_file_type="tar.gz"
max_archives=3 # Set the maximum number of archives to keep
archive_backup_dir="backups"

## Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NO_COLOR='\033[0m'

# Function
## restart emacs (taken from bashrc)
re() {
    echo "Restarting Emacs daemon..."
    systemctl --user restart emacs || echo "${RED}Failed reboot emacs daemon${NO_COLOR}"
    echo "${GREEN}Done!"
}

# Script
echo "Backup started..."

## Navigate to the configuration directory
cd ~/.config/ || exit

mkdir -p "${archive_backup_dir}"

## Check for existing archive files
archives=$(ls ${archive_prefix}*${archive_file_type}* 2>/dev/null)
archive_count=0

for _ in $archives; do
    archive_count=$((archive_count + 1))
done

# limit amount of archives
if [ "$archive_count" -gt 0 ]; then
    # Check if the number of archives exceeds the limit
    if [ "$archive_count" -ge "$max_archives" ]; then
        # Identify and remove the oldest archive
        oldest_archive=$(ls -1t ${archive_prefix}*${archive_file_type}* | tail -n 1)

        if [ -n "$oldest_archive" ]; then
            rm "$oldest_archive" && echo "${YELLOW}Removed oldest archive: $oldest_archive${NO_COLOR}"
        fi
    fi
fi

## Get the current Git HEAD SHA-1
git_sha=$(git -C "$(pwd)/emacs" rev-parse --short HEAD)

archive_name="${archive_prefix}_$(date +%Y-%m-%d_%H-%M-%S)_${git_sha}.${archive_file_type}"

## Create a new archive file
tar --create --gzip --file "$(pwd)/backups/${archive_name}" "${archive_prefix}/"

echo "${GREEN}Backup end. Starting upgrade...${NO_COLOR}"

## Uncomment the following lines to enable the upgrade functionality
doom upgrade --aot || echo "${RED}Failed upgrade doom${NO_COLOR}"

doom sync --gc || echo "${RED}Failed to sync doom post-upgrade${NO_COLOR}"

echo "${GREEN}Upgrade done..${NO_COLOR}"

## Call the restart function and handle failure
re || {
    echo "${RED}Restart Emacs command not available.${NO_COLOR}"
    exit 1
}
