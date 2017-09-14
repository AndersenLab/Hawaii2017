#!/usr/bin/env python
# @Author: Daniel E. Cook
# This script is used to generate collection labels.
import labels
from reportlab.graphics import shapes
from reportlab.graphics.barcode import eanbc
import qrcode
from tempfile import mktemp


people = ['R']

label_types = {
    "a_5963": labels.Specification(210, 297, 2, 5, 102, 54, corner_radius=2,
                                   top_margin=8,
                                   row_gap=2,
                                   left_margin=0,
                                   right_margin=0),
    "a_5267": labels.Specification(215.90, 297.40, 4, 20, 47, 14, corner_radius=2,
                                   top_margin=8,
                                   row_gap=0,
                                   left_margin=0,
                                   right_margin=0),

}

#===================#
# Collection Labels #
#===================#

def draw_label(label, width, height, obj):
    # Generate QR Code for labels
    qr_barcode = mktemp(suffix='.png')
    qrcode.make(str(obj['ID'])).save(qr_barcode, "png")
    label.add(shapes.Image(0, 80, 70, 70, qr_barcode))
    # Add ID, date, time
    label.add(shapes.String(70, 128, str(
        obj['ID']), fontName="Courier", fontSize=18))
    #label.add(shapes.String(70, 100, str("08 / __ / 17"),
    #                        fontName="Courier", fontSize=14))
    #label.add(shapes.String(200, 100, str("___:___"),
    #                        fontName="Courier", fontSize=14))
#
    #label.add(shapes.String(10, 60, str(
    #    "Lat;Lon    __________;__________"), fontName="Courier", fontSize=14))
#
    ## Substrate
    #label.add(shapes.String(10, 40, str(
    #    "Substrate  __________________"), fontName="Courier", fontSize=14))
#
    ## Humidity
    #label.add(shapes.String(10, 20, str("Humidity   _____%"),
    #                        fontName="Courier", fontSize=14))

specs = label_types['a_5963']
sheet = labels.Sheet(specs, draw_label, border=False)

# Generate large labels
for person in people:
    for i in range(0, 500):
        _id = person + "-" + str("%04i" % (i + 1))
        sheet.add_label({"ID": _id})
sheet.save('collection-forms.pdf')
print("{0:d} label(s) output on {1:d} page(s).".format(
    sheet.label_count, sheet.page_count))


#====================#
# Plating Out Labels #
#====================#

def draw_label_plate(label, width, height, obj):
    # Generate QR Code for labels
    qr_barcode = mktemp(suffix='.png')

    qrcode.make(str(obj['ID'])).save(qr_barcode, "png")
    label.add(shapes.Image(3, -2, 44, 44, qr_barcode))
    label.add(shapes.String(45, 16, str(
        obj['ID']), fontName="Courier", fontSize=16))


specs = label_types['a_5267']
sheet = labels.Sheet(specs, draw_label_plate, border=False)

# Generate large labels
for person in people:
    for i in range(0, 500):
        _id = person + "-" + str("%04i" % (i + 1))
        sheet.add_label({"ID": _id})

sheet.save('plating-labels.pdf')
print("{0:d} label(s) output on {1:d} page(s).".format(
    sheet.label_count, sheet.page_count))
