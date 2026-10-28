# LPP - Locate Profile Point in Plan

Civil 3D AutoLISP command that maps a station selected in a profile view to plan and creates a perpendicular XLINE through the alignment at that station.

## Use

Load `LPP_LocatePlanPoint.lsp` with `APPLOAD`, or add it to the Startup Suite. Run `LPP` after selecting an alignment, profile view, or profile line; alternatively select one when prompted. Pick one or more points in the profile view and press Enter to finish.

The command derives the perpendicular direction from alignment points at offsets on either side of the station. It creates XLINEs on `XL-LINES` with ACI color 8 without changing the current layer or color.

## Compatibility and status

Designed for Civil 3D on Windows. Version 1.2.2 needs field verification, particularly the profile-view station lookup and behavior near alignment endpoints. Profile views are assumed to be unrotated in the WCS plane. Use a copy of a drawing for initial tests.

## License

MIT. See [LICENSE](./LICENSE).
