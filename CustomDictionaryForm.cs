using System;
using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;

namespace uclliu
{
    public class CustomDictionaryForm : Form
    {
        private readonly string customJsonPath;
        private readonly Action reloadWordRoot;
        private readonly Dictionary<string, List<string>> entries;
        private readonly ListView listView;
        private readonly TextBox keyTextBox;
        private readonly TextBox valueTextBox;
        private string editingKey = "";
        private string editingValue = "";
        private bool isFilteringKey = false;

        public CustomDictionaryForm(string customJsonPath, Action reloadWordRoot, Icon icon)
        {
            this.customJsonPath = customJsonPath;
            this.reloadWordRoot = reloadWordRoot;
            entries = CustomDictionaryStore.Load(customJsonPath, null);

            Text = "肥米自定字詞功能";
            Width = 620;
            Height = 430;
            StartPosition = FormStartPosition.CenterScreen;
            FormBorderStyle = FormBorderStyle.FixedSingle;
            MaximizeBox = false;
            MinimizeBox = false;
            ShowIcon = true;
            if (icon != null)
            {
                Icon = icon;
            }

            TableLayoutPanel root = new TableLayoutPanel();
            root.Dock = DockStyle.Fill;
            root.ColumnCount = 1;
            root.RowCount = 3;
            root.Padding = new Padding(10);
            root.RowStyles.Add(new RowStyle(SizeType.Absolute, 90));
            root.RowStyles.Add(new RowStyle(SizeType.Percent, 100));
            root.RowStyles.Add(new RowStyle(SizeType.Absolute, 44));
            Controls.Add(root);

            TableLayoutPanel editor = new TableLayoutPanel();
            editor.Dock = DockStyle.Fill;
            editor.ColumnCount = 4;
            editor.RowCount = 2;
            editor.ColumnStyles.Add(new ColumnStyle(SizeType.Absolute, 70));
            editor.ColumnStyles.Add(new ColumnStyle(SizeType.Absolute, 130));
            editor.ColumnStyles.Add(new ColumnStyle(SizeType.Absolute, 70));
            editor.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 100));
            editor.RowStyles.Add(new RowStyle(SizeType.Absolute, 30));
            editor.RowStyles.Add(new RowStyle(SizeType.Percent, 100));
            root.Controls.Add(editor, 0, 0);

            Label keyLabel = new Label();
            keyLabel.Text = "字根";
            keyLabel.TextAlign = ContentAlignment.MiddleLeft;
            editor.Controls.Add(keyLabel, 0, 0);

            keyTextBox = new TextBox();
            keyTextBox.MaxLength = 5;
            keyTextBox.Dock = DockStyle.Fill;
            keyTextBox.TextChanged += keyTextBox_TextChanged;
            editor.Controls.Add(keyTextBox, 1, 0);

            Label valueLabel = new Label();
            valueLabel.Text = "字詞";
            valueLabel.TextAlign = ContentAlignment.MiddleLeft;
            editor.Controls.Add(valueLabel, 2, 0);

            valueTextBox = new TextBox();
            valueTextBox.Multiline = true;
            valueTextBox.ScrollBars = ScrollBars.Vertical;
            valueTextBox.Dock = DockStyle.Fill;
            editor.SetRowSpan(valueTextBox, 2);
            editor.Controls.Add(valueTextBox, 3, 0);

            Label hintLabel = new Label();
            hintLabel.Text = "允許 a-z , . ] [ '，最多 5 碼";
            hintLabel.TextAlign = ContentAlignment.MiddleLeft;
            hintLabel.ForeColor = Color.DimGray;
            editor.Controls.Add(hintLabel, 0, 1);
            editor.SetColumnSpan(hintLabel, 3);

            listView = new ListView();
            listView.Dock = DockStyle.Fill;
            listView.View = View.Details;
            listView.FullRowSelect = true;
            listView.GridLines = true;
            listView.HideSelection = false;
            listView.Columns.Add("字根", 120);
            listView.Columns.Add("字詞", 440);
            listView.SelectedIndexChanged += listView_SelectedIndexChanged;
            root.Controls.Add(listView, 0, 1);

            FlowLayoutPanel buttons = new FlowLayoutPanel();
            buttons.Dock = DockStyle.Fill;
            buttons.FlowDirection = FlowDirection.RightToLeft;
            root.Controls.Add(buttons, 0, 2);

            Button closeButton = CreateButton("關閉", closeButton_Click);
            Button downButton = CreateButton("下移", downButton_Click);
            Button upButton = CreateButton("上移", upButton_Click);
            Button deleteButton = CreateButton("刪除", deleteButton_Click);
            Button clearButton = CreateButton("清空", clearButton_Click);
            Button saveButton = CreateButton("新增/更新", saveButton_Click);

            buttons.Controls.Add(closeButton);
            buttons.Controls.Add(downButton);
            buttons.Controls.Add(upButton);
            buttons.Controls.Add(deleteButton);
            buttons.Controls.Add(clearButton);
            buttons.Controls.Add(saveButton);

            RefreshList();
        }

        protected override void OnFormClosing(FormClosingEventArgs e)
        {
            SaveAndReload();
            base.OnFormClosing(e);
        }

        private Button CreateButton(string text, EventHandler handler)
        {
            Button button = new Button();
            button.Text = text;
            button.Width = 88;
            button.Height = 30;
            button.Click += handler;
            return button;
        }

        private void saveButton_Click(object sender, EventArgs e)
        {
            string key = CustomDictionaryStore.NormalizeRootKey(keyTextBox.Text);
            string value = valueTextBox.Text.Trim();
            if (!CustomDictionaryStore.IsValidRootKey(key))
            {
                MessageBox.Show(this, "字根只能使用 a-z , . ] [ '，最多 5 碼。", "肥米自定字詞功能", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                keyTextBox.Focus();
                return;
            }
            if (value.Length == 0)
            {
                MessageBox.Show(this, "請輸入要自定的字詞。", "肥米自定字詞功能", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                valueTextBox.Focus();
                return;
            }

            if (editingKey.Length > 0 && entries.ContainsKey(editingKey))
            {
                entries[editingKey].Remove(editingValue);
                if (entries[editingKey].Count == 0)
                {
                    entries.Remove(editingKey);
                }
            }

            if (!entries.ContainsKey(key))
            {
                entries[key] = new List<string>();
            }
            if (!entries[key].Contains(value))
            {
                entries[key].Add(value);
            }

            editingKey = "";
            editingValue = "";
            keyTextBox.Text = "";
            valueTextBox.Text = "";
            SaveAndReload();
            RefreshList();
        }

        private void deleteButton_Click(object sender, EventArgs e)
        {
            if (listView.SelectedItems.Count == 0)
            {
                return;
            }

            string key = listView.SelectedItems[0].SubItems[0].Text;
            string value = listView.SelectedItems[0].SubItems[1].Text;
            if (entries.ContainsKey(key))
            {
                entries[key].Remove(value);
                if (entries[key].Count == 0)
                {
                    entries.Remove(key);
                }
            }
            editingKey = "";
            editingValue = "";
            keyTextBox.Text = "";
            valueTextBox.Text = "";
            SaveAndReload();
            RefreshList();
        }

        private void upButton_Click(object sender, EventArgs e)
        {
            MoveSelected(-1);
        }

        private void downButton_Click(object sender, EventArgs e)
        {
            MoveSelected(1);
        }

        private void clearButton_Click(object sender, EventArgs e)
        {
            editingKey = "";
            editingValue = "";
            keyTextBox.Text = "";
            valueTextBox.Text = "";
            keyTextBox.Focus();
        }

        private void closeButton_Click(object sender, EventArgs e)
        {
            Close();
        }

        private void listView_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (listView.SelectedItems.Count == 0)
            {
                return;
            }

            editingKey = listView.SelectedItems[0].SubItems[0].Text;
            editingValue = listView.SelectedItems[0].SubItems[1].Text;
            keyTextBox.Text = editingKey;
            valueTextBox.Text = editingValue;
        }

        private void keyTextBox_TextChanged(object sender, EventArgs e)
        {
            if (isFilteringKey)
            {
                return;
            }

            string filtered = FilterKey(keyTextBox.Text);
            if (filtered != keyTextBox.Text)
            {
                isFilteringKey = true;
                int selectionStart = Math.Min(filtered.Length, keyTextBox.SelectionStart);
                keyTextBox.Text = filtered;
                keyTextBox.SelectionStart = selectionStart;
                isFilteringKey = false;
            }
        }

        private string FilterKey(string value)
        {
            value = CustomDictionaryStore.NormalizeRootKey(value);
            string filtered = "";
            for (int i = 0; i < value.Length && filtered.Length < 5; i++)
            {
                string c = value[i].ToString();
                if (CustomDictionaryStore.IsValidRootKey(c))
                {
                    filtered += c;
                }
            }
            return filtered;
        }

        private void MoveSelected(int direction)
        {
            if (listView.SelectedItems.Count == 0)
            {
                return;
            }

            string key = listView.SelectedItems[0].SubItems[0].Text;
            string value = listView.SelectedItems[0].SubItems[1].Text;
            if (!entries.ContainsKey(key))
            {
                return;
            }

            int index = entries[key].IndexOf(value);
            int nextIndex = index + direction;
            if (index < 0 || nextIndex < 0 || nextIndex >= entries[key].Count)
            {
                return;
            }

            entries[key].RemoveAt(index);
            entries[key].Insert(nextIndex, value);
            SaveAndReload();
            RefreshList(key, value);
        }

        private void SaveAndReload()
        {
            CustomDictionaryStore.Save(customJsonPath, entries);
            if (reloadWordRoot != null)
            {
                reloadWordRoot();
            }
        }

        private void RefreshList()
        {
            RefreshList("", "");
        }

        private void RefreshList(string selectedKey, string selectedValue)
        {
            listView.BeginUpdate();
            listView.Items.Clear();
            List<string> keys = new List<string>(entries.Keys);
            keys.Sort(StringComparer.Ordinal);
            foreach (string key in keys)
            {
                foreach (string value in entries[key])
                {
                    ListViewItem item = new ListViewItem(key);
                    item.SubItems.Add(value);
                    listView.Items.Add(item);
                    if (key == selectedKey && value == selectedValue)
                    {
                        item.Selected = true;
                        item.Focused = true;
                    }
                }
            }
            listView.EndUpdate();
        }
    }
}
