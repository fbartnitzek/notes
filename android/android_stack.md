# android-stack:
- Application Layer (every installed app)
- Application Framework
- C/C++ Libs / Android Runtime (dalvik/art)
- Linux Kernel

build with gradle:
1) Android Project
2) gradle: build
3) Gradle (Byte Code, Resources, Manifest) => APK
4) Jar Signer: sign
5) ADB (Android Debug Bridge): Install on Device
```
adb install -r ...
adb shell ...
runs app
```
