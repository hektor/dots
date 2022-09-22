# Scripts

Mostly tiny helper scripts & experiments, some more useful than others.

## Setup

Make sure the scripts are in your `$PATH` and executable.

To add the complete directory of scripts to your `$PATH`:

```bash
export PATH=~/.bin:$PATH
```

To make a any script `<script>` executable:
```bash
chmod +x <script>
```

## Notes

Statusbar scripts are prefixed with `sb` under the assumption you're using
`dwmblocks` (scripts my need changes to be used with different statusbars).

Some scripts have dependencies (e.g. `fzf`, `jq`, `xclip`, ...), make sure you
have them installed.
