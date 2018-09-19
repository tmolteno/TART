#!/bin/sh
chmod 0400 /id_rsa

autossh -M 0 -N -T -C -i /id_rsa \
            -o StrictHostKeyChecking=no \
            -o ExitOnForwardFailure=yes \
            -R ${SSH_TUNNEL_REMOTE}:${SSH_TUNNEL_HOST}:${SSH_TUNNEL_LOCAL} \
            ${SSH_HOSTUSER}@${SSH_HOSTNAME}
