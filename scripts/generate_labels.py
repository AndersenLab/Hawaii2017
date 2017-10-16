#!/usr/bin/env python
# @Author: Daniel E. Cook
# This script is used to generate collection labels.
import labels
from reportlab.graphics import shapes
from reportlab.graphics.barcode import eanbc
import qrcode
from tempfile import mktemp


# Configuration
LABEL_USE = "a_61533" # Type of label
LABEL_SETS = {'C': 120, 'S': 240} # Label prefixes and counts



LABEL_TYPE = {
    "a_5963": labels.Specification(210, 297, 2, 5, 102, 54, corner_radius=2,
                                   top_margin=8,
                                   row_gap=2,
                                   left_margin=0,
                                   right_margin=0),

    "a_5267": labels.Specification(210, 297, 4, 20, 47, 14, corner_radius=2,
                                   top_margin=8,
                                   row_gap=0,
                                   left_margin=0,
                                   right_margin=0),

    "a_61533": labels.Specification(210, 297, 4, 15, 44.45, 17.5, corner_radius=2,
                                   top_margin=10,
                                   row_gap=1,
                                   left_margin=0,
                                   right_margin=0),

}
#====================#
# Generate labels    #
#====================#

def draw_label_plate(label, width, height, obj):
    # Generate QR Code for labels
    qr_barcode = mktemp(suffix='.png')

    qrcode.make(str(obj['ID'])).save(qr_barcode, "png")
    label.add(shapes.Image(3, 0, 44, 44, qr_barcode))
    label.add(shapes.String(45, 16, str(
        obj['ID']), fontName="Courier", fontSize=16))



# Generate large labels
for label, n in LABEL_SETS.items():
    specs = LABEL_TYPE[LABEL_USE]
    sheet = labels.Sheet(specs, draw_label_plate, border=False)
    for i in range(0, n):
        _id = label + "-" + str("%04i" % (i + 1))
        sheet.add_label({"ID": _id})

    sheet.save('{}-labels.pdf'.format(label))
    print("{0:d} label(s) output on {1:d} page(s).".format(
        sheet.label_count, sheet.page_count))
