package com.sksamuel.scapegoat

import com.sksamuel.scapegoat.inspections._
import com.sksamuel.scapegoat.inspections.collections._
import com.sksamuel.scapegoat.inspections.controlflow.WhileTrue
import com.sksamuel.scapegoat.inspections.empty._
import com.sksamuel.scapegoat.inspections.equality._
import com.sksamuel.scapegoat.inspections.exception._
import com.sksamuel.scapegoat.inspections.imports.DuplicateImport
import com.sksamuel.scapegoat.inspections.inference._
import com.sksamuel.scapegoat.inspections.matching._
import com.sksamuel.scapegoat.inspections.math._
import com.sksamuel.scapegoat.inspections.naming._
import com.sksamuel.scapegoat.inspections.nulls._
import com.sksamuel.scapegoat.inspections.option._
import com.sksamuel.scapegoat.inspections.string._
import com.sksamuel.scapegoat.inspections.style._
import com.sksamuel.scapegoat.inspections.unneccesary._
import com.sksamuel.scapegoat.inspections.unsafe._

/** @author Stephen Samuel */
object ScapegoatConfig extends App {

  def inspections: Seq[Inspection] = Seq(
    ArrayEquals,
    ArraysInFormat,
    ArraysToString,
    AsInstanceOf,
    AvoidOperatorOverload,
    AvoidSizeEqualsZero,
    AvoidSizeNotEqualsZero,
    AvoidToMinusOne,
    BigDecimalDoubleConstructor,
    BigDecimalScaleWithoutRoundingMode,
    BoundedByFinalType,
    BrokenOddness,
    CatchException,
    CatchFatal,
    CatchNpe,
    CatchThrowable,
    ClassNames,
    CollectionIndexOnNonIndexedSeq,
    CollectionNamingConfusion,
    CollectionNegativeIndex,
    CollectionPromotionToAny,
    ComparingFloatingPointTypes,
    ComparingUnrelatedTypes,
    ComparisonToEmptyList,
    ComparisonToEmptySet,
    ComparisonWithSelf,
    ConstantIf,
    DivideByOne,
    DoubleNegation,
    DuplicateImport,
    DuplicateMapKey,
    DuplicateSetValue,
    EitherGet,
    EmptyCaseClass,
    EmptyFor,
    EmptyIfBlock,
    EmptyInterpolatedString,
    EmptyMethod,
    EmptySynchronizedBlock,
    EmptyTryBlock,
    EmptyWhileBlock,
    ExistsSimplifiableToContains,
    FilterDotHead,
    FilterDotHeadOption,
    FilterDotIsEmpty,
    FilterDotSize,
    FilterOptionAndGet,
    FinalModifierOnCaseClass,
    FinalizerWithoutSuper,
    FindAndNotEqualsNoneReplaceWithExists,
    FindDotIsDefined,
    IllegalFormatString,
    ImpossibleOptionSizeCondition,
    IncorrectNumberOfArgsToFormat,
    IncorrectlyNamedExceptions,
    InvalidRegex,
    IsInstanceOf,
    JavaConversionsUse,
    ListAppend,
    ListSize,
    LonelySealedTrait,
    LooksLikeInterpolatedString,
    MapGetAndGetOrElse,
    MaxParameters,
    MethodNames,
    MethodReturningAny,
    ModOne,
    NanComparison,
    NegationIsEmpty,
    NegationNonEmpty,
    NoOpOverride,
    NullAssignment,
    NullParameter,
    ObjectNames,
    OptionGet,
    OptionSize,
    ParameterlessMethodReturnsUnit,
    PartialFunctionInsteadOfMatch,
    PointlessTypeBounds,
    PreferSeqEmpty,
    PreferSetEmpty,
    ProductWithSerializableInferred,
    PublicFinalizer,
    RedundantFinalModifierOnMethod,
    RedundantFinalModifierOnVar,
    RedundantFinalizer,
    RepeatedCaseBody,
    ReverseFunc,
    ReverseTailReverse,
    ReverseTakeReverse,
    SimplifyBooleanExpression,
    StripMarginOnRegex,
    SubstringZero,
    SuspiciousMatchOnClassObject,
    SwallowedException,
    SwapSortFilter,
    TraversableHead,
    TraversableLast,
    TryGet,
    TypeShadowing,
    UnnecessaryIf,
    UnnecessaryReturnUse,
    UnnecessaryToInt,
    UnnecessaryToString,
    UnreachableCatch,
    UnsafeContains,
    UnusedMethodParameter,
    UseCbrt,
    UseExpM1,
    UseLog10,
    UseLog1P,
    UseSqrt,
    VarClosure,
    VarCouldBeVal,
    WhileTrue,
    ZeroNumerator)
}
