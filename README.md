# Hundred Rabbits themes

Decode theme files and use themes that conform to the [Hundred Rabbits theme framework](https://github.com/hundredrabbits/Themes) in your application.

### Tiny example:

```

import Xml.Decode as XD -- from ymtszw/elm-xml-decode
import HRTheme

hundredRabbitsTheme = XD.run HRTheme.decoder xmlString

```