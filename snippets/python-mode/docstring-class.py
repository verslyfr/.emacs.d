# -*- mode: snippet -*-
# name: docstring-class
# key: dsclass>
# group: docstring
# --
    """${1:The summary line for a class docstring should fit on one line.}

    ${2:If the class has public attributes, they may be documented here in an
    ``Attributes`` section and follow the same formatting as a function's
    ``Args`` section. It is recommended to document Attributes inline where
    they are declared though.

    Properties created with the ``@property`` decorator should be documented
    in the property's getter method.}
    """