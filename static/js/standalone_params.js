// Author: Paul Rouse
// Copyright 2017 University of Bristol
// Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org

// When integrated with CEAP, this is actually not used at all, but is
// needed to keep the static checks in Yesod happy.  Stand-alone installations
// will have it overwritten by the installer to give a site-specific version.
const homeHash       = '/';     // Part of URL for "home" page after hash sign
const standAloneIdle = 600;     // Idle timeout in seconds
