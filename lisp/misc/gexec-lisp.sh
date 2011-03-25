#!/bin/bash

export LD_ASSUME_KERNEL=2.4.0
/usr/mill/pkg/acl/alisp -I hrl.dxl -L $1 -kill -- ${GEXEC_MY_VNN}
