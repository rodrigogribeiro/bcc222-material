# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
  config.vm.box = "archlinux/archlinux"

  config.vm.provider "virtualbox" do |vb|
    vb.memory = "8048"
    vb.cpus = 4
  end

  config.vm.provision "shell", inline: <<-SHELL
    # Update the system
    sudo pacman -Syu --noconfirm

    # Install necessary packages
    sudo pacman -S --noconfirm base-devel git ghc cabal-install stack emacs

    # Set up Emacs for TTY mode
    echo "(setq inhibit-startup-message t)" >> /etc/skel/.emacs
    echo "(menu-bar-mode -1)" >> /etc/skel/.emacs
    echo "(tool-bar-mode -1)" >> /etc/skel/.emacs
    echo "(scroll-bar-mode -1)" >> /etc/skel/.emacs
    echo "(setq visible-bell t)" >> /etc/skel/.emacs
    echo "(setq ring-bell-function 'ignore)" >> /etc/skel/.emacs

    # Set up Haskell enviroment 
    cabal update
    stack setup

  SHELL

  config.vm.provision "shell", path: "build-and-run.sh"

end

