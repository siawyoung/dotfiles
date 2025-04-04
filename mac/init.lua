hs.loadSpoon("Lunette")

hs.hotkey.bind({"alt"}, "1", function()
    hs.application.launchOrFocus("Vivaldi")
end)
hs.hotkey.bind({"alt"}, "3", function()
    hs.application.launchOrFocus("iTerm")
end)
hs.hotkey.bind({"alt"}, "4", function()
    hs.application.launchOrFocus("Slack")
end)
hs.hotkey.bind({"alt"}, "z", function()
    hs.application.launchOrFocus("Telegram")
end)
hs.hotkey.bind({"alt"}, "w", function()
    hs.application.launchOrFocus("Cursor")
end)
hs.hotkey.bind({"alt"}, "e", function()
    hs.application.launchOrFocus("Google Chrome")
end)

hs.spoons.use("Lunette", { hotkeys = "default" })

-- Open Vivaldi, then switch to third tab
hs.hotkey.bind({"alt", "shift"}, "c", function()
    hs.application.launchOrFocus("Vivaldi")

    -- Give Vivaldi a moment to come into focus
    hs.timer.doAfter(0.5, function()
        hs.eventtap.keyStroke({"cmd"}, "3", 0)
    end)
end)

hs.hotkey.bind({"alt", "shift"}, "m", function()
    hs.application.launchOrFocus("Vivaldi")

    -- Give Vivaldi a moment to come into focus
    hs.timer.doAfter(0.5, function()
        hs.eventtap.keyStroke({"cmd"}, "4", 0)
    end)
end)

hs.hotkey.bind({"alt", "shift"}, "l", function()
    hs.application.launchOrFocus("Vivaldi")

    -- Give Vivaldi a moment to come into focus
    hs.timer.doAfter(0.5, function()
        hs.eventtap.keyStroke({"cmd"}, "5", 0)
    end)
end)
