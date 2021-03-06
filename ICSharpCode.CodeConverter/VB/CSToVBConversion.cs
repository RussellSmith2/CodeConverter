﻿using System.Collections.Generic;
using System.Linq;
using ICSharpCode.CodeConverter.CSharp;
using ICSharpCode.CodeConverter.Shared;
using ICSharpCode.CodeConverter.Util;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Text;
using VBSyntaxFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory;
using CSSyntaxFactory = Microsoft.CodeAnalysis.CSharp.SyntaxFactory;
using CSSyntax = Microsoft.CodeAnalysis.CSharp.Syntax;
using VBSyntax = Microsoft.CodeAnalysis.VisualBasic.Syntax;

namespace ICSharpCode.CodeConverter.VB
{
    public class CSToVBConversion : ILanguageConversion
    {
        public SyntaxTree SingleFirstPass(Compilation sourceCompilation, SyntaxTree tree)
        {
            var converted = CSharpConverter.ConvertCompilationTree((CSharpCompilation)sourceCompilation, (CSharpSyntaxTree)tree);
            var convertedTree = VBSyntaxFactory.SyntaxTree(converted);
            return convertedTree;
        }

        public SyntaxNode GetSurroundedNode(IEnumerable<SyntaxNode> descendantNodes,
            bool surroundedWithMethod)
        {
            return surroundedWithMethod
                ? descendantNodes.OfType<CSSyntax.MethodDeclarationSyntax>().First<SyntaxNode>()
                : descendantNodes.OfType<CSSyntax.BaseTypeDeclarationSyntax>().First<SyntaxNode>();
        }

        public bool MustBeContainedByMethod(SyntaxNode node)
        {
            return node is CSSyntax.IncompleteMemberSyntax || 
                   node is CSSyntax.StatementSyntax || 
                   CouldBeFieldOrLocalVariableDeclaration(node); ;
        }

        public bool MustBeContainedByClass(SyntaxNode node)
        {
            return node is CSSyntax.BaseMethodDeclarationSyntax || node is CSSyntax.BaseFieldDeclarationSyntax ||
                   node is CSSyntax.BasePropertyDeclarationSyntax;
        }

        private static bool CouldBeFieldOrLocalVariableDeclaration(SyntaxNode node)
        {
            return node is CSSyntax.FieldDeclarationSyntax f && f.Modifiers.All(m => m.IsKind(SyntaxKind.TypeVarKeyword));
        }

        public string WithSurroundingMethod(string text)
        {
            return $@"void SurroundingSub()
{{
{text}
}}";
        }

        public string WithSurroundingClass(string text)
        {
            return $@"class SurroundingClass
{{
{text}
}}";
        }

        public List<SyntaxNode> FindSingleImportantChild(SyntaxNode annotatedNode)
        {
            var children = annotatedNode.ChildNodes().ToList();
            if (children.Count > 1) {
                switch (annotatedNode) {
                    case VBSyntax.TypeBlockSyntax typeBlock:
                        return typeBlock.Members.ToList<SyntaxNode>();
                    case VBSyntax.MethodBlockBaseSyntax methodBlock:
                        return methodBlock.Statements.ToList<SyntaxNode>();
                }
            }
            return children;
        }

        public SyntaxNode SingleSecondPass(KeyValuePair<string, SyntaxTree> cs)
        {
            return cs.Value.GetRoot();
        }

        public string GetWarningsOrNull()
        {
            return null;
        }

        public SyntaxTree CreateTree(string text)
        {
            return CSSyntaxFactory.ParseSyntaxTree(SourceText.From(text));
        }

        public Compilation CreateCompilationFromTree(SyntaxTree tree, IEnumerable<MetadataReference> references)
        {
            return CSharpCompilation.Create("Conversion", new[] { tree }, references);
        }
    }
}