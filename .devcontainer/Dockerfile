FROM ubuntu:22.04

# Evitar prompts interactivos
ENV DEBIAN_FRONTEND=noninteractive

# Instalar dependencias básicas
RUN apt-get update && apt-get install -y \
    software-properties-common \
    curl \
    git \
    wget \
    build-essential \
    # SWI-Prolog
    swi-prolog \
    # Python
    python3 \
    python3-pip \
    python3-venv \
    # GUI dependencies (para Tkinter)
    python3-tk \
    # X11 para GUI (opcional, si usas WSL)
    x11-apps \
    && rm -rf /var/lib/apt/lists/*

# Crear enlace simbólico python -> python3
RUN ln -s /usr/bin/python3 /usr/bin/python

# Instalar PySwip
RUN pip3 install --no-cache-dir pyswip

# Crear directorio de trabajo
WORKDIR /workspace

# Usuario no-root (opcional pero recomendado)
ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID

RUN groupadd --gid $USER_GID $USERNAME \
    && useradd --uid $USER_UID --gid $USER_GID -m $USERNAME \
    && apt-get update \
    && apt-get install -y sudo \
    && echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME \
    && chmod 0440 /etc/sudoers.d/$USERNAME

USER $USERNAME
