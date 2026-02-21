clabs <- c(`109` = "109th Congress (Bush, Hastert, Frist)",
           `110` = "110th Congress (Bush, Pelosi, Reid)",
           `111` = "111th Congress (Obama, Pelosi, Reid)",
           `112` = "112nd Congress (Obama, Boehner, Reid)",
           `113` = "113rd Congress (Obama, Boehner, Reid)",
           `114` = "114th Congress (Obama, Boehner/Ryan, McConnell)",
           `115` = "115th Congress (Trump, Ryan, McConnell)")


clabs.graph <- c("109\nBush\nHastert\nFrist",
                 "110\nBush\nPelosi\nReid",
                 "111\nObama\nPelosi\nReid",
                 "112\nObama\nBoehner\nReid",
                 "113\nObama\nBoehner\nReid",
                 "114\nObama\nBoehner-Ryan\nMcConnell")


congEra <- tibble(cong = 109:114,
                  name = clabs.graph,
                  start = c(2006.0, 2006.2, 2008.2, 2010.2, 2012.2, 2014.2),
                  end   = c(2006.2, 2008.2, 2010.2, 2012.2, 2014.2, 2016.2),
                  name.x = c(2006, 2007.2, 2009.2, 2011.2, 2013.2, 2015.2),
                  name.y = rep(0.6, 6),
                  type  = "",
                  pty4  = "foo")

cong.color <- c(`109` = "orangered3",
                `110` = "royalblue3",
                `111` = "royalblue3",
                `112` = "purple3",
                `113` = "purple3",
                `114` = "orangered3")
