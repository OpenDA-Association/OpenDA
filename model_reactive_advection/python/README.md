## build executable

Dependencies

```bat
    pip install pyinstaller
```

First run the default option to create a one folder build
```bat
    pyinstaller src/reactive_pollution_model.py
```

To create a single executable use
```bat
    pyinstaller --onefile src/reactive_pollution_model.py
```

The resulting folder/executable are located in the `dist` folder.
Note that when running the option with `--onefile` after running the one folder build you need to clean the `dist` folder manually. 