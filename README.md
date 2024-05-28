# Brave Pose GUI

*GUI framework for those who feel brave*

This project aims to be CSS based MVVM library using OpenGL as Haskell doesn't have any other GUI library. 

![Test example](https://github.com/SturdyPose/brave-pose-gui/actions/workflows/haskell.yml/badge.svg?event=push)

## Why Haskell?

Haskell provides **safety, performance, ease of testability** and many advanced features other languages don't dispose such as Monads, Lenses, advanced meta-programming and ease of concurrency.
Also a lot of beginners might want to try Haskell out and the terminal as only option is a sure way not to learn Haskell anymore.
This framework is for those who want tools for writing games or other GUI applications. Writing styles in code only can be a huge hassle and CSS is by far the best option for writing anything modern looking.

We haskellers love this language (even with some caveats) and this repo is a way of showing that it's not just a meme.

## Feature Checklist:

- [ ] Instanced rendering for performance
- [ ] Add XML based rendering
- [ ] Full MVVM support with templating
- [ ] CSS support
- [ ] Advanced testing features (Selenium question mark?)
- [X] Basic features with all graphic vendors support - for example no geometry shaders or drawing with GLLines
- [X] Cross platform
- [ ] Decent test coverage

## Prerequisites: 

- GPU with latest OpenGL 4.6 support (this might be downgraded down the line if features like Tesselation shaders won't be needed)
- Haskell stack


## Ubuntu setup:

Tests happen using Ubuntu machine and these deps were necessary to run the project.
On the desktop Ubuntu you might find some libs unnecessary. If so, please open an item in GitHub so we can resolve this.

```bash 
sudo apt update
sudo apt-get install -y freeglut3 freeglut3-dev libgl1-mesa-glx libxi-dev libxrandr-dev libxxf86vm-dev libxcursor-dev libxinerama-dev 
```

## Windows setup:

Main development is on a windows machine, but to be fair, this needs to be most likely updated since it was a long time I did this.
Stack works with msys2 behind the scenes and it uses pacman package manager.

```powershell
stack exec -- pacman -Syu
stack exec -- pacman -S mingw-w64-freeglut
```

## Build and Test:

`stack build`

`stack test`