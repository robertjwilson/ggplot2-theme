
## 18/01/2016

Some changes have been made to make the theme behave a bit better and to be consistent with v. 2 of ggplot2.

1. plot_type has been removed as an option. It was really just letting you do something that could be done easily anyway.
2. The ability to easily remove axis ticks has been added with the ticks_xy option.
3. ticks_type has been changed because axis.ticks.margin is being deprecated.
4. ticks_length is no longer something that can be modified.
5. Some minor aesthetic changes have been made to make the defaults look a bit better.


## 9/10/2015

theme_agile is now updated in response to some of the feedback after I put it on GitHub a week ago.

1. I have switched from . to _ in the theme options to be consistent. Having base_size and plot.type was probably confusing.

2. The base fone size has been switched to 11 to match the default ggplot2 size.

3. I have added in an option (grid_thick) to make grid lines thinner or thicker.
