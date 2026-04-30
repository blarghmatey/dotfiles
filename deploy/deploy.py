"""Pyinfra orchestration entry point.

Imports and calls each scoped component module in order. Components are
responsible for their own idempotency via pyinfra operations.

Usage:
    pyinfra @local deploy/deploy.py
    pyinfra @local deploy/deploy.py --data profile=arch-wsl2
"""

from deploy.components.node_tools import install_node_tools
from deploy.components.packages import install_packages
from deploy.components.python_tools import install_python_tools

install_packages()
install_python_tools()
install_node_tools()
