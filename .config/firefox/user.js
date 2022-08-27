/*
 * Note: has to be symlinked to profile directories for your
 *       firefox release
 *
 * E.g.
 *
 * ```sh
 * ln -s user.js ~/.mozilla/firefox/*.default-release/user.js
 * ```
 * Or check out the `setup` script
 *
 */

user_pref('browser.download.dir', '/home/h/dl')
