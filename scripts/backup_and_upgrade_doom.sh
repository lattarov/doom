#!/usr/bin/env sh

# Introduction
## Script to backup the emacs directory, where doom Emacs
## binaries are installed and where all caches are stored.

# Tasks
## TODO use absolute paths
## TODO use doom environment variables
## TODO get environment variables from doom emacs
## TODO merge with doom emacs scripts

# Constants
## Archive
archive_prefix="emacs"
archive_file_type="tar.gz"
archive_count_max=3
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

echo "Backup started..."

cd ~/.config/ || exit

mkdir -p "${archive_backup_dir}"

## Limit amount of archives
archives=$(ls "${archive_backup_dir}"/${archive_prefix}*${archive_file_type}* 2>/dev/null)
archive_count=0

for _ in $archives; do
    archive_count=$((archive_count + 1))
done

if [ "$archive_count" -gt 0 ]; then
    # Check if the number of archives exceeds the limit
    if [ "$archive_count" -ge "$archive_count_max" ]; then
        # Identify and remove the oldest archive
        oldest_archive=$(ls -1t "${archive_backup_dir}"/${archive_prefix}*${archive_file_type}* | tail -n 1)

        if [ -n "$oldest_archive" ]; then
            rm "$oldest_archive" && echo "${YELLOW}Removed oldest archive: $oldest_archive${NO_COLOR}"
        fi
    fi
fi

## Get the current Git HEAD SHA-1
git_sha=$(git -C "$(pwd)/emacs" rev-parse --short HEAD)

archive_name="${archive_prefix}_$(date +%Y-%m-%d_%H-%M-%S)_${git_sha}.${archive_file_type}"

## Generate backup
tar --create --gzip --file "$(pwd)/backups/${archive_name}" "${archive_prefix}/"

echo "${GREEN}Backup end. Starting upgrade...${NO_COLOR}"

## Upgrade, native compile, sync and cleanup
doom upgrade --force --aot --jobs ${nproc-1} || echo "${RED}Failed upgrade doom!${NO_COLOR}"

doom sync --jobs ${nproc-1} || echo "${RED}Failed to sync after upgrade!${NO_COLOR}"

doom gc --force || echo "${RED}Failed to garbage-collect after upgrade!${NO_COLOR}"

echo "${GREEN}Upgrade done!${NO_COLOR}"

re || {
    echo "${RED}Restart Emacs command not available!${NO_COLOR}"
    exit 1
}
