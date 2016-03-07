# build tips
```
ProjectDir$ chmod +x gradlew
ProjectDir$ ./gradlew assembleDebug  // apk-file build below app/build/
```

```
install: adb install -r app/build/apk/app-debug-unaligned.apk
start: adb shell am start -n com.example.android.sunshine.app/com.example.android.sunshine.app.MainActivity
```
