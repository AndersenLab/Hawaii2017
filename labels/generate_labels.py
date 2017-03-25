import labels
from reportlab.graphics import shapes
import qrcode
from tempfile import mktemp

# Create an A4 portrait (210mm x 297mm) sheets with 2 columns and 8 rows of
# labels. Each label is 90mm x 25mm with a 2mm rounded corner. The margins are
# automatically calculated.
specs = labels.Specification(210, 297, 2, 8, 90, 25, corner_radius=2)

# Create a function to draw each label. This will be given the ReportLab drawing
# object to draw on, the dimensions (NB. these will be in points, the unit
# ReportLab uses) of the label, and the object to render.
def draw_label(label, width, height, obj):
    # Generate QR Code for labels
    qr_barcode = mktemp(suffix='.png')
    qrcode.make(str(obj['ID'])).save(qr_barcode, "png")
    label.add(shapes.Image(0, 0, 70, 70, qr_barcode))
    # Add ID
    label.add(shapes.String(70, 50, str(obj['ID']), fontName="Helvetica", fontSize=15))
    label.add

# Create the sheet.
sheet = labels.Sheet(specs, draw_label, border=True)

# Add a couple of labels.
sheet.add_label({"ID":"Identifier #1"})
sheet.add_label({"ID":"Identifier #2"})

# Save the file and we are done.
sheet.save('basic.pdf')
print("{0:d} label(s) output on {1:d} page(s).".format(sheet.label_count, sheet.page_count))