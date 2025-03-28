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

hs.spoons.use("Lunette", { hotkeys = "default" })

