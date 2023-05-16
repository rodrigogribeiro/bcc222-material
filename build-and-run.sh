#!/bin/bash

# Test Haskell environment
cd /vagrant/aula09
sudo -u vagrant stack exec aula09-exe

if [ ! -d "/vagrant/slides" ]; then
    mkdir /vagrant/slides
fi

# Clone org-reveal repository
if [ ! -d "/home/vagrant/.emacs.d/org-reveal" ]; then
git clone https://github.com/yjwen/org-reveal.git /home/vagrant/.emacs.d/org-reveal
fi

# Set up Emacs for org-reveal
sudo -u vagrant echo "(add-to-list 'load-path (expand-file-name \"org-reveal\" \"~/.emacs.d\"))" >> /home/vagrant/.emacs
sudo -u vagrant echo "(require 'ox-reveal)" >> /home/vagrant/.emacs

# Install org-reveal package
sudo -u vagrant emacs --batch -l /home/vagrant/.emacs -eval '(load-library "ox-reveal")'

# Define function to export slides
echo "(defun export-slides (file) (find-file file) (org-reveal-export-to-html))" >> /home/vagrant/export.el

# Create reveal presentation
for dir in /vagrant/aula* /vagrant/extra*; do
  if [ -d "$dir" ]; then
    for file in "$dir"/*.org; do
      if [ -f "$file" ]; then
        sudo -u vagrant emacs --batch -l /home/vagrant/.emacs -l /home/vagrant/export.el --eval "(export-slides \"$file\")" 
        mv "${file%.org}.html" /vagrant/slides/
      fi
    done
  fi
done


