/*-----------------------------------------------------------------------

    Manual tracking

    Original plugin by
        Fabrice P Cordelières, fabrice.cordelieres at curie.u-psud.fr

    Modifications by Jean-Olivier Irisson, for DISCUS -- 2009

-----------------------------------------------------------------------*/

import java.awt.*;
import java.awt.event.*;
import java.awt.SystemColor;
import java.io.*;
import java.lang.*;
import java.util.StringTokenizer;
import ij.*;
import ij.gui.*;
import ij.io.*;
import ij.measure.*;
import ij.plugin.Converter;
import ij.plugin.frame.*;
import ij.plugin.filter.*;
import ij.plugin.filter.Duplicater;
import ij.process.*;
import ij.process.StackConverter;
import ij.util.*;
import ij.util.Tools.*;

public class Manual_Tracking extends PlugInFrame implements ActionListener, ItemListener, MouseListener {

    //Universal variables-------------------------------------------------------
    int i;
    int j;
    int k;
    int l;
    int m;
    int n;
    String txt;


    //Interface related variables-----------------------------------------------
    static Frame instance;
    Font bold = new Font("",3,12);
    Panel panel;
    //Tracking
    Button butAdd;
    Button butDlp;
    Button butEnd;
    Button butDel;
    Choice trackdel;
    Button butDelAll;
    Checkbox checkPath;


    //Image related variables---------------------------------------------------
    ImagePlus img;
    String imgtitle;
    int Width;
    int Height;
    int Depth;
    int Slice;
    String SliceTitle;
    ImageCanvas canvas;
    ImagePlus ip;
    ImageStack stack;
    ImageWindow win;
    StackConverter sc;
    Duplicater dp;


    //Tracking related variables------------------------------------------------
    boolean islistening=false; //True as long as the user is tracking

    int[] xRoi; //Defines the ROI to be shown using the 'Show path' option - x coordinates
    int[] yRoi; //Defines the ROI to be shown using the 'Show path' option - y coordinates
    Roi roi;    // ROI to be shown using the 'Show path' option
    int Nbtrack=1; // Number of tracks
    int NbPoint=1; // Number of tracked points in the current track
    int ox;     // x coordinate of the current tracked point
    int oy;     // y coordinate of the current tracked point


    //Dialog boxes--------------------------------------------------------------
    GenericDialog gd;


    //Results tables------------------------------------------------------------
    ResultsTable rt;    //2D results table
    ResultsTable rtmp;  // Temporary results table
    // results table's heading
    String[] head={"trackNb","sliceNb","imgNb","x","y"};


    public Manual_Tracking() {

        //Interface setup ------------------------------------------------------
        super("Manual tracking");
        instance=this;

        panel = new Panel();
        panel.setLayout(new GridLayout(0,2, 5, 5));
        panel.setBackground(SystemColor.control);

        butAdd = new Button("Add track");
        butAdd.addActionListener(this);
        panel.add(butAdd);

        butEnd = new Button("End track");
        butEnd.addActionListener(this);
        panel.add(butEnd);

        butDlp = new Button("Delete last point");
        butDlp.addActionListener(this);
        panel.add(butDlp);

        checkPath=new Checkbox("Show path ?", false);
        checkPath.addItemListener(this);
        panel.add(checkPath);

        butDel = new Button("Delete track nb");
        butDel.addActionListener(this);
        panel.add(butDel);

        trackdel = new Choice();
        panel.add(trackdel);

        butDelAll = new Button("Delete all tracks");
        butDelAll.addActionListener(this);
        panel.add(butDelAll);

        add(panel,BorderLayout.CENTER);
        pack();
        show();

        IJ.showProgress(2,1);

        rt=new ResultsTable();
    }

    public void itemStateChanged(ItemEvent e) {
        // Show/Hide the current path-------------------------------------------
        if (e.getSource() == checkPath) {
            if (checkPath.getState()) {img.setRoi(roi);
            } else {
                img.killRoi();
            }
        }
    }

    public void actionPerformed(ActionEvent e) {
        // Button Add Track pressed---------------------------------------------
        if (e.getSource() == butAdd) {
            // Check whether the stack is already being followed
            if (islistening){
                IJ.showMessage("This operation can't be completed:\na track is already being followed...");
                return;
            }

            // Prepare the stack
            img=WindowManager.getCurrentImage();
            stack = img.getStack();
            imgtitle = img.getTitle();
            if (imgtitle.indexOf(".")!=-1) imgtitle=imgtitle.substring(0,imgtitle.indexOf("."));
            // Set point tool
            IJ.setTool(7);

            // Arrays to store the coordinates of points to draw a polyline at the last step
            xRoi=new int[img.getStackSize()];
            yRoi=new int[img.getStackSize()];

            // Check for stack existence
            if (img==null){
                IJ.showMessage("Error", "Man,\n"+"You're in deep troubles:\n"+"no opened stack...");
                return;
            }

            // Go to the first slice
            win = img.getWindow();
            canvas=win.getCanvas();
            img.setSlice(1);

            // Initialiwe counters
            NbPoint=1;
            IJ.showProgress(2,1);

            // Listen to mouse events
            canvas.addMouseListener(this);
            islistening=true;
            return;
        }

        // Button Delete last point pressed-------------------------------------
        if (e.getSource() == butDlp) {
            // Ask the user
            gd = new GenericDialog("Delete last point");
            gd.addMessage("Are you sure you want to\n" + "delete last point ?");
            gd.showDialog();
            if (gd.wasCanceled()) return;

            // Create a temporary ResultTable and copy data from the original
            rtmp=new ResultsTable();
            for (i=0; i<(rt.getCounter()); i++) {
                rtmp.incrementCounter();
                for (j=0; j<head.length; j++) rtmp.addValue(j, rt.getValue(j,i));
            }

            // Clean original table
            rt.reset();

            // Set header back
            for (i=0; i<head.length; i++) rt.setHeading(i,head[i]);

            //Copy data back to original table except last point
            for (i=0; i<((rtmp.getCounter())-1); i++) {
                rt.incrementCounter();
                for (j=0; j<head.length; j++) rt.addValue(j, rtmp.getValue(j,i));
            }
            rt.show("Tracks");

            //Manage case where the deleted point is the last of a serie
            if (islistening==false) {
                Nbtrack--;
                trackdel.remove(""+(int) rt.getValue(0,rt.getCounter()-1));
                canvas.addMouseListener(this);
                islistening=true;
            }

            // Decrement the point counter
            NbPoint--;

            // Set stack to previous slice
            img.setSlice(((int) rt.getValue(1, rt.getCounter()-1))+1);

            IJ.showStatus("Last Point Deleted !");
        }

        // Button End Track pressed---------------------------------------------
        if (e.getSource() == butEnd) {
            trackdel.add(""+Nbtrack);
            Nbtrack++;
            canvas.removeMouseListener(this);
            islistening=false;
            IJ.showStatus("Tracking is over");
            IJ.showProgress(2,1);
            return;
        }

        // Button Del Track pressed---------------------------------------------
        if (e.getSource() == butDel) {
            // Ask user whether the current track should be stopped
            if (islistening) {
                gd = new GenericDialog("Delete Track");
                gd.addMessage("This will end current track\n" + "Do you want to continue?");
                gd.showDialog();
                if (gd.wasCanceled()) return;
            }

            // Stop tracking
            trackdel.add(""+Nbtrack);
            Nbtrack++;
            canvas.removeMouseListener(this);
            islistening=false;

            // Fecth track number
            int tracktodelete= (int) Tools.parseDouble(trackdel.getItem(trackdel.getSelectedIndex()));

            // Ask the user for confirmation again
            gd = new GenericDialog("Delete Track nb" + tracktodelete);
            gd.addMessage("Do you want to \n" + "delete track nb" + tracktodelete + " ?");
            gd.showDialog();
            if (gd.wasCanceled()) return;

            // Create a temporary ResultTable and copy data from the original except for the deleted track
            rtmp=new ResultsTable();
            for (i=0; i<(rt.getCounter()); i++) {
                int nbtrack=(int) rt.getValue(0,i);
                if(nbtrack!=tracktodelete){
                    rtmp.incrementCounter();
                    for (j=0; j<head.length; j++) rtmp.addValue(j, rt.getValue(j,i));
                }
            }

            rt.reset();

            // Copy data back to original table
            for (i=0; i<head.length; i++) rt.setHeading(i,head[i]);

            for (i=0; i<(rtmp.getCounter()); i++) {
                rt.incrementCounter();
              for (j=0; j<head.length; j++){
                    if (j==0 & rtmp.getValue(0,i)>tracktodelete){
                        rt.addValue(j, rtmp.getValue(j,i)-1);
                    } else {
                        rt.addValue(j, rtmp.getValue(j,i));
                    }
                }
            }

            rt.show("Tracks");

            // Reset the drop down choices for deleting the tracks
            trackdel.removeAll();
            // If there are tracks left
            if ((rt.getCounter()) != 0) {
                // add them back as choices
                for (i=1;i<(rt.getValue(0,rt.getCounter()-1))+1;i++){
                    trackdel.add(""+i);
                }
                // reset track counter
                Nbtrack=((int) rt.getValue(0,rt.getCounter()-1))+1;
            } else {
                // If there are no tracks left, just set track counter to 1
                Nbtrack=1;
            }

            IJ.showStatus("Track nb"+ tracktodelete +" Deleted !");

        }

        // Button Del All Tracks pressed----------------------------------------
        if (e.getSource() == butDelAll) {
            // Ask the user
            if (islistening) {
                gd = new GenericDialog("Delete Track");
                gd.addMessage("This will also delete current track\n" + "Do you want to continue?");
                gd.showDialog();
                if (gd.wasCanceled()) return;
            }

            // Stop tracking
            canvas.removeMouseListener(this);
            islistening=false;
            IJ.showProgress(2,1);
            IJ.showStatus("Tracking is over");

            // Ask the user for confirmation again
            gd = new GenericDialog("Delete All Tracks");
            gd.addMessage("Do you want to \n" + "delete all measurements ?");
            gd.showDialog();
            if (gd.wasCanceled()) return;

            // Reset result table
            rt.reset();
            rt.show("Tracks");
            trackdel.removeAll();
            IJ.showStatus("All Tracks Deleted !");

            // Reset track counter
            Nbtrack=1;
            return;
        }

    }

    // Click on image-----------------------------------------------------------
    public void mouseReleased(MouseEvent m) {

        // Status message in the main window
        IJ.showProgress(img.getCurrentSlice()+1,img.getStackSize()+1);
        IJ.showStatus("Tracking slice "+(img.getCurrentSlice()+1)+" of "+(img.getStackSize()+1));

        // If this is the first point, set the results table heading
        if (Nbtrack==1 && NbPoint==1){
            for (i=0; i<head.length; i++) rt.setHeading(i,head[i]);
        }

        // Suppress possible ROIs
        img.killRoi();

        // Get clicked coordinates
        int x=m.getX();
        int y=m.getY();
        ox=canvas.offScreenX(x);
        oy=canvas.offScreenY(y);

        xRoi[NbPoint-1]=ox;
        yRoi[NbPoint-1]=oy;

        // Go to the next line in the results table
        rt.incrementCounter();

        // Detect the filename of the image (which is a number) and add that to the output table
        int sliceNb=img.getCurrentSlice();
        String label=stack.getSliceLabel(sliceNb);
        // System.out.println(label);
        // System.out.println(label.indexOf("."));
        // System.out.println(label.substring(0,label.indexOf(".")));
        double imgNb=Double.parseDouble(label.substring(0,label.indexOf(".")));

        // Prepare the output line (only double numbers)
        // NB: invert Y coordinate
        double[] doub={Nbtrack,(img.getCurrentSlice()),imgNb,ox,img.getHeight()-oy};

        // Add the line to the results table
        for (i=0; i<doub.length; i++) rt.addValue(i,doub[i]);
        rt.show("Tracks");

        if ((img.getCurrentSlice())<img.getStackSize()) {
            // If we are not at the last slice:

            // go to the next slice
            img.setSlice(img.getCurrentSlice()+1);
            // increase counter of points for polyline
            NbPoint++;
            // add point to the polyline
            roi=new PolygonRoi(xRoi,yRoi,NbPoint-1,Roi.POLYLINE);
            // display the polyline if requested
            if(checkPath.getState()) img.setRoi(roi);

        } else {
            // If this is the last slice:

            // add a new track
            trackdel.add(""+Nbtrack);
            Nbtrack++;
            // add last point to the polyline
            roi=new PolygonRoi(xRoi,yRoi,NbPoint,Roi.POLYLINE);
            // display the polyline
            img.setRoi(roi);
            // stop tracking
            canvas.removeMouseListener(this);
            islistening=false;
            IJ.showStatus("Tracking is over");
            return;
        }

    }

    public void mousePressed(MouseEvent m) {}
    public void mouseExited(MouseEvent m) {}
    public void mouseClicked(MouseEvent m) {}
    public void mouseEntered(MouseEvent m) {}

}

