object Form1: TForm1
  Left = 192
  Top = 107
  AutoSize = True
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Form1'
  ClientHeight = 329
  ClientWidth = 529
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 529
    Height = 329
    ActivePage = TabSheet3
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Customers'
      object lbCustomers: TListBox
        Left = 8
        Top = 8
        Width = 241
        Height = 281
        ItemHeight = 13
        TabOrder = 0
        OnClick = lbCustomersClick
      end
      object GroupBox1: TGroupBox
        Left = 256
        Top = 8
        Width = 249
        Height = 249
        Caption = 'Details'
        TabOrder = 1
        object Label1: TLabel
          Left = 16
          Top = 16
          Width = 31
          Height = 13
          Caption = 'Name:'
        end
        object Label2: TLabel
          Left = 16
          Top = 32
          Width = 45
          Height = 13
          Caption = 'Surname:'
        end
        object lblName: TLabel
          Left = 80
          Top = 16
          Width = 3
          Height = 13
        end
        object lblSurname: TLabel
          Left = 80
          Top = 32
          Width = 3
          Height = 13
        end
        object Label3: TLabel
          Left = 16
          Top = 88
          Width = 32
          Height = 13
          Caption = 'Tasks:'
        end
        object lbCustomerTasks: TListBox
          Left = 24
          Top = 112
          Width = 209
          Height = 121
          ItemHeight = 13
          TabOrder = 0
        end
      end
      object btnAddCustomer: TButton
        Left = 256
        Top = 264
        Width = 75
        Height = 25
        Caption = 'Add'
        TabOrder = 2
        OnClick = btnAddCustomerClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Tasks'
      ImageIndex = 1
      object lbTasks: TListBox
        Left = 8
        Top = 32
        Width = 241
        Height = 257
        ItemHeight = 13
        TabOrder = 0
        OnClick = lbTasksClick
      end
      object btnNewTask: TButton
        Left = 256
        Top = 264
        Width = 75
        Height = 25
        Caption = 'New Task'
        TabOrder = 1
        OnClick = btnNewTaskClick
      end
      object cbCustomers: TComboBox
        Left = 8
        Top = 8
        Width = 241
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 2
        OnClick = cbCustomersClick
      end
      object gbEditTask: TGroupBox
        Left = 256
        Top = 8
        Width = 241
        Height = 249
        Caption = 'Edit task'
        TabOrder = 3
        Visible = False
        object cbTaskPriority: TComboBox
          Left = 16
          Top = 24
          Width = 209
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = cbTaskPriorityChange
          Items.Strings = (
            'Low'
            'Normal'
            'High')
        end
        object CheckListBox1: TCheckListBox
          Left = 16
          Top = 72
          Width = 209
          Height = 161
          OnClickCheck = CheckListBox1ClickCheck
          ItemHeight = 13
          TabOrder = 1
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'About'
      ImageIndex = 2
      DesignSize = (
        521
        301)
      object Label4: TLabel
        Left = 8
        Top = 8
        Width = 21
        Height = 13
        Caption = '****'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label5: TLabel
        Left = 16
        Top = 32
        Width = 489
        Height = 65
        AutoSize = False
        Caption = 
          'I hope this example is closer to '#39'real-life'#39' cases than the demo' +
          's provided with the Framework itself. '#13#10'If you improve that exam' +
          'ple in any way, or know the better way for solving any of the pr' +
          'oblems, please share it!'
        WordWrap = True
      end
      object Label6: TLabel
        Left = 8
        Top = 248
        Width = 156
        Height = 39
        Anchors = [akLeft, akBottom]
        Caption = 
          'Written by Michal '#39'migajek'#39' Gajek'#13#10'migajek@gmail.com'#13#10'http://mig' +
          'ajek.com/'
      end
      object Label7: TLabel
        Left = 288
        Top = 272
        Width = 229
        Height = 13
        Cursor = crHandPoint
        Anchors = [akRight]
        Caption = 'http://code.google.com/p/synopse-sqlite-demo/'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = Label7Click
      end
    end
  end
end
