FROM base/archlinux
RUN pacman -Sy
RUN pacman -S --needed base-devel --noconfirm
RUN pacman -S stack --noconfirm
RUN pacman -S z3 --noconfirm
RUN pacman -S maven jdk8-openjdk --noconfirm
RUN sed -i /etc/locale.gen -e '/en_US.UTF-8/ s/#//'
RUN locale-gen