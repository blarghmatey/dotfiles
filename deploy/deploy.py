"""Pyinfra orchestration entry point.

Imports and calls each scoped component module in order. Components are
responsible for their own idempotency via pyinfra operations.

Cargo and Go components are disabled by default; pass ``enable_cargo=true``
or ``enable_go=true`` via ``--data`` to activate them.

Usage:
    pyinfra @local deploy/deploy.py
    pyinfra @local deploy/deploy.py --data profile=arch-wsl2
    pyinfra @local deploy/deploy.py --data enable_cargo=true
    pyinfra @local deploy/deploy.py --data enable_go=true
"""

from deploy.components.cargo_tools import install_cargo_tools
from deploy.components.go_tools import install_go_tools
from deploy.components.node_tools import install_node_tools
from deploy.components.packages import install_packages
from deploy.components.python_tools import install_python_tools

install_packages()
install_python_tools()
install_node_tools()
install_cargo_tools()
install_go_tools()
