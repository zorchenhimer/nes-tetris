InitScores:
    jsr ClearSprites

    ldx #0
    jsr FillAttributeTable

    ldx #0
    jsr FillScreen

FrameScores:
    jsr WaitForNMI
    jmp FrameMenu
