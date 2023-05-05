<div>
    <img align="left" height="120" src="./assets/ufop.png">
    <p> 
        Universidade Federal de Ouro Preto
        <br>
        Disciplina: BCC222 - Programação Funcional
        <br>
        Professor: Rodrigo Geraldo Ribeiro
    </p>
</div>
<hr />


### Primeiros passos

#### 1.0 - Desenvolvendo Haskell

##### 1.1 Configurando o ambiente

###### 1.1.1 Instalando diretamente em uma máquina

- Instale o [Haskell Stack](https://docs.haskellstack.org/en/stable/).

###### 1.1.2 Desenvolvendo num ambiente Vagrant

- Copie e cole o seguinte Vagrantfile na pasta onde deseja ter seu ambiente, ou use o seu preferido:

<details open>
    <summary> Vagarantfile </summary>

``` ruby

# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
  config.vm.box = "archlinux/archlinux"

  config.vm.provider "virtualbox" do |vb|
    vb.memory = "2048"
    vb.cpus = 2
  end

  config.vm.provision "shell", inline: <<-SHELL
    # Update the system
    sudo pacman -Syu --noconfirm

    # Install necessary packages
    sudo pacman -S --noconfirm base-devel git ghc cabal-install stack

    # Set up a new user for Haskell development
    sudo useradd -m -G wheel haskelldev
    echo 'haskelldev ALL=(ALL) NOPASSWD: ALL' | sudo EDITOR='tee -a' visudo

    # Switch to the new user and set up the Haskell environment
    sudo su - haskelldev -c "cabal update"
    sudo su - haskelldev -c "stack setup"
  SHELL
end

```

- Certifique-se de que o repositorio está no mesmo diretorio ou é um filho de onde está inserindo o seu Vagrantfile.
- Execute os comandos:

``` bash

vagrant up
vagrant ssh

```

- Agora, dentro do container:

``` bash

cd /vagrant
ls

```

- Se você encontrou os diretorios filhos da sua pasta no host tudo está funcionando normalmente.
- Lembre-se, por estar num ambiente sem interface gráfica, você terá que usar um editor de texto com [(n)vim](https://github.com/neovim/neovim) ou [emacs](www.gnu.org/software/emacs/).

</details>

##### 1.2 Testando o setup de desenvolvimento em Haskell

- No terminal, acesse a pasta aula09 e execute os comandos:

``` bash 
stack build 

stack exec aula09-exe
```

se a execução acontecer com sucesso, sua máquina 
está devidamente configurada.

<hr/>

<div align="center" >
    <img src="./assets/static-vs-dynamic.jpg" />

**Happy hacking!**

</div>
