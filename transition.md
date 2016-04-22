| Function       | replaced by           |
| ------------- |:-------------|
| `append_side` | `modify_side` |
| `views_with_side` | nothing. Only used in `print` for checking axes right now |
| `set_args` | something more explicit for the separation of arguments |
| `function_args` | this is useful, but maybe should be renamed and def documented |
| `set_inherited_args` | replace with something else that deals with embedded function args |
| `set_window` | phase this out with `add_to_view`, `modify_side`, and `modify_par` |
| `calc_views` | breaking this into multiple functions |
| `group_views` | using `add_to_view` |
| `append_replace` | keep this as a utility function? or what? |
| `which_reals` | not used anymore. Delete |
| `set_view_window` | only used in `set_view_lab`, which will be replaced by `modify_side` |
| `set_view_lab` | replace w/ `modify_side` |
| `remove_field` | this obscures some things and we probably want some more explicit methods. Delete/replace? |
| `strip_pts` | needs to be improved if it is useful to keep |
| `summarize_side_values` | hopefully we can improve this or do it elsewhere |
| `set_sides` | confusing name, but this deals w/ turning c(1) into c(1,2) |
| `non_views` | we should phase this out, as our model will only contain sides, views, and global at top |

