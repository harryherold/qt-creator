#pragma once

constexpr auto SCROLLBAR_STYLE = R"(
      QScrollBar{
        background: palette(base);
        width: 20px
      }
      QScrollBar:horizontal{
        margin: 0px 0px 0px 0px;
      }
      QScrollBar:vertical{
        margin: 0px 0px 0px 0px;
      }
      QScrollBar::handle{
        background: palette(alternate-base);
        border: 1px solid;
        border-radius: 1px;
        border-color: palette(highlight);
      }
      QScrollBar::handle:hover, QScrollBar::add-line:hover, QScrollBar::sub-line:hover{
        background-color: palette(highlight);
      }
      QScrollBar::add-line{
        subcontrol-origin: none;
      }
      QScrollBar::add-line:vertical, QScrollBar::sub-line:vertical{
        height: 0px;
      }
      QScrollBar::add-line:horizontal, QScrollBar::sub-line:horizontal{
        width: 0px;
      }
      QScrollBar::sub-line{
        subcontrol-origin: none;
      }
)";
