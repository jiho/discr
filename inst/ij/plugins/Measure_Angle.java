/*

    Plugin for the detection of the angle of objects in a stack

    Inspired by Manual_tracking from Fabrice P Cordelières

    (c) Copyright 2014 Jean-Olivier Irisson
        GNU General Public License v3

*/

import java.awt.*;
import java.awt.event.*;
import ij.*;
import ij.IJ.*;
import ij.gui.*;
import ij.measure.*;
import ij.plugin.frame.*;
import ij.process.*;
import java.awt.TextArea;

public class Measure_Angle extends PlugInFrame implements ActionListener {

    int i;
    ImagePlus imp;

    Button buttonMeasure;
    Button buttonNext;
    Button buttonPrev;
    Button buttonDelete;

    public Measure_Angle() {

        // Interface setup
        super("Measure angle");
        Frame instance=this;

        Panel panel = new Panel();
        // setup a grid with x lines and 1 column
        panel.setLayout(new GridLayout(4, 1));
        // panel.setLayout(new GridLayout(5, 1));
        panel.setBackground(SystemColor.control);

        // TextArea ta = new TextArea("Draw a line then click 'Get Angle'", 1, 25, TextArea.SCROLLBARS_NONE);
        // ta.setEditable(false);
        // panel.add(ta);

        buttonMeasure = new Button("Get Angle ⌘M");
        buttonMeasure.addActionListener(this);
        panel.add(buttonMeasure);

        buttonNext = new Button("Next Image >");
        buttonNext.addActionListener(this);
        panel.add(buttonNext);

        buttonPrev = new Button("Previous Image <");
        buttonNext.addActionListener(this);
        panel.add(buttonPrev);

        buttonDelete = new Button("Delete last measurement");
        buttonDelete.addActionListener(this);
        panel.add(buttonDelete);

        add(panel, BorderLayout.CENTER);
        pack();
        setVisible(true);

        // Set tool to line
        IJ.setTool("line");
        IJ.run("Set Measurements...", " stack redirect=None decimal=3");

    }

    public void actionPerformed(ActionEvent e) {

        if (e.getSource() == buttonMeasure) {
            IJ.run(imp, "Measure", "");
        }

        if (e.getSource() == buttonNext) {
            IJ.run(imp, "Next Slice [>]", "");
        }
        
        if (e.getSource() == buttonNext) {
            IJ.run(imp, "Previous Slice [<]", "");
        }

        if (e.getSource() == buttonDelete) {
            // confirmation dialog
            GenericDialog gd = new GenericDialog("Delete last point");
            gd.addMessage("Are you sure you want to\n" + "delete last point ?");
            gd.showDialog();
            if ( gd.wasCanceled() ) return;

            // remove the last tow of the table
            ResultsTable rt = ResultsTable.getResultsTable();
            i = rt.getCounter();
            IJ.showStatus(Integer.toString(i));
            rt.deleteRow(i-1);
            rt.show("Results");

            IJ.showStatus("Last measure deleted !");
        }

    }

}

