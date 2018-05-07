#!/usr/bin/env bash

set -euo pipefail

HOST_UID="$(stat -c %u Dockerfile)"
HOST_GID="$(stat -c %g Dockerfile)"

USER=arksink

# UID ranges
# ==========
#
# Only go ahead if we are not root on the host. There's always a chance
# that a user with an ID in the system range runs this, although it's a
# slim one; the chance increases if the host has a smaller range; macOS
# has a range of 0–500, for example, which is lower than Debian's 0–1k,
# although I do not know if Debian has any system groups that use an ID
# that high. Of course, if the size of the ranges match but they aren't
# aligned, that could cause problems, and probably many other scenarios
# not considered.
#
# Not only could the UID be in the system range, it may well be in use.
# Again, hopefully not too likely, but if that's the case, there is not
# much I can do.
#
# Since I'm only writing this for myself, I'm not too worried about it,
# but it's not impossible that I'm not the only user.
if [ 0 -ne $HOST_UID ]; then
  (
    set +e

    usermod -u $HOST_UID $USER

    # Change the user group to the host's GID,
    original_output="$(groupmod -g $HOST_GID $USER 2>&1)"

    if [ 4 -eq $? ]; then
      # If the group ID already exists (exit status 4), it is not good.
      # We'll add the user to the group with the host GID all the same,
      # and can only hope that is not (too much of) a problem; it means
      # there's a group in the system that happens to correspond to the
      # host's group ID already and since this is based on a default OS
      # image, that can only be a system group, as I have added none of
      # my own users to the image.
      #
      # Until Docker — and docker-compose — supports dynamic mapping of
      # the host UID & GID that is running the container to an internal
      # UID & GID, this is the best solution I am currently aware of to
      # prevent writes on a host-bound volume as root.
      #
      # We could probably just change the group ID of the current group
      # to a random available one, but I suppose that might cause other
      # problems if it goes from an ID within the system-reserved range
      # to one above it. I don't know if it would, but either way, risk
      # of changing basic system expectations is a problem.
      #
      # Still, I would go with the second risk were it not for the fact
      # that it would require changing the group owner of every file on
      # the system that is owned by the group whose ID I am changing to
      # the new group ID (which costs time), or that I'd have take care
      # not to change the group owner of the files on the volume (which
      # adds complexity to an already complex shuffle).
      usermod --append --groups $HOST_GID $USER
    elif [ 0 -ne $? ]; then
      # Any other error should just be forwarded.
      >&2 echo "$original_output"
      exit $?
    fi
  )
fi

exec sudo -EH -u $USER -- $@
